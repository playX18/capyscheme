//! `Masm` (macro assembler): `onlineasm::Assembler` plus SABI helpers.

use onlineasm::{Assembler, Extern, ExternalName, Label, Target, VReg, onlineasm};

use super::image::UnloadedImage;
use crate::runtime::sabi::{self, RpCompactHeader, builtin_class_ids, regs};
use crate::runtime::value::Value;
use crate::utils::flags;

pub const EXTERN_NAMESPACE_RUNTIME: u32 = 1;
#[cfg(test)]
pub const EXTERN_NAMESPACE_TEST: u32 = 2;
/// Direct prim wrappers, indexed by prim id (see
/// [`PRIM_WRAPPERS`](crate::runtime::prims::calls::PRIM_WRAPPERS)).
pub const EXTERN_NAMESPACE_PRIM: u32 = 3;

/// Declares [`RuntimeExtern`]: one line per Rust helper generated code
/// calls, giving its variant, symbol and address. The kinds follow JSC's
/// split:
///
/// - `control`: JSC's `CommonSlowPaths`. `extern "C" fn(th) -> *const u8`
///   reads the staged `Tc` state, may change frames, and returns the
///   machine address to continue at. Reached through [`SlowPath`].
/// - `operation`: JSC's `JITOperations`. Typed arguments in, a result out,
///   and the caller continues where it left off. Reached through
///   [`TemplateContext::call_operation`]. The prim wrappers in
///   [`EXTERN_NAMESPACE_PRIM`] are operations too, numbered by prim id.
/// - `stub`: helpers only generated stubs and glues call, each with its
///   own convention.
///
/// [`SlowPath`]: super::template::SlowPath
/// [`TemplateContext::call_operation`]: super::template::TemplateContext::call_operation
macro_rules! runtime_externs {
    ($($kind:ident { $($variant:ident => $($path:ident)::+),* $(,)? })*) => {
        /// A Rust helper generated code can call. The discriminant is the
        /// extern index within [`EXTERN_NAMESPACE_RUNTIME`].
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        #[repr(u32)]
        pub enum RuntimeExtern {
            $($($variant,)*)*
        }

        impl RuntimeExtern {
            pub const ALL: &'static [RuntimeExtern] = &[$($(RuntimeExtern::$variant,)*)*];
            pub const COUNT: usize = Self::ALL.len();

            /// The helper's symbol, for disassembly and the debugger.
            pub fn name(self) -> &'static str {
                match self {
                    $($(Self::$variant => runtime_externs!(@last $($path)::+),)*)*
                }
            }

            /// The helper's address, resolved at load.
            pub fn address(self) -> *const u8 {
                use super::{lazy, slow, stubs};
                match self {
                    $($(Self::$variant => $($path)::+ as *const u8,)*)*
                }
            }
        }
    };
    (@last $head:ident $(:: $tail:ident)+) => { runtime_externs!(@last $($tail)::+) };
    (@last $name:ident) => { stringify!($name) };
}

runtime_externs! {
    control {
        HotEnter => slow::capy_hot_enter,
        YieldLoop => slow::capy_yield_loop,
        HotLoop => slow::capy_hot_loop,
        Overflow => slow::capy_overflow,
        Arity => slow::capy_arity,
        CallSlow => slow::capy_call_slow,
        ReturnMv => slow::capy_return_mv,
        Raise => slow::capy_raise,
        RaiseKind => slow::capy_raise_kind,
        NumberArith => slow::capy_number_arith,
        CompareJump => slow::capy_compare_jump,
        VectorOp => slow::capy_vector_op,
        VectorLength => slow::capy_vector_length,
        PrimN => slow::capy_prim_n,
        PrimError => slow::capy_prim_error,
        GetSlot => slow::capy_get_slot,
        PutSlot => slow::capy_put_slot,
        RuntimeProc => slow::capy_runtime_proc,
        Nop => slow::capy_nop,
    }
    operation {
        Alloc => slow::capy_op_alloc,
        RestList => slow::capy_op_rest_list,
        ValuesRest => slow::capy_op_values_rest,
        LogObject => slow::capy_log_object,
    }
    stub {
        Bail => stubs::capy_bail,
        LazyHit => lazy::capy_lazy_hit,
        IcMissCall => stubs::capy_ic_miss_call,
        IcMissTcall => stubs::capy_ic_miss_tcall,
        SubrCall => slow::capy_subr_call,
        SubrRet => slow::capy_subr_ret,
        Underflow => slow::capy_underflow,
        RaiseRet => slow::capy_raise_ret,
        Exit => slow::capy_exit,
        ValuesError => slow::capy_values_error,
    }
}

impl RuntimeExtern {
    /// The assembler key for this helper.
    pub fn external(self) -> ExternalName {
        ExternalName::user(EXTERN_NAMESPACE_RUNTIME, self as u32)
    }

    /// The helper an assembler key names, if it is a runtime helper.
    pub fn from_external(name: &ExternalName) -> Option<Self> {
        let ExternalName::User(key) = name else {
            return None;
        };
        if key.namespace != EXTERN_NAMESPACE_RUNTIME {
            return None;
        }
        Self::ALL.get(key.index as usize).copied()
    }
}

/// A numeric comparison. Fixnum fast paths compare as signed 32-bit
/// integers; flonum fast paths compare ordered doubles, so NaN compares
/// false under every variant.
#[derive(Clone, Copy)]
pub enum Cmp {
    Lt,
    Le,
    Eq,
    Ge,
    Gt,
}

/// What a recorded symbol names
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SymbolKind {
    Function,
    Handler,
    Glue,
    Stub,
}

/// A named address range, published to the debugger at load.
pub struct SymbolRecord {
    pub name: String,
    pub start: Label,
    pub end: Label,
    pub kind: SymbolKind,
}

/// One `CFA = sp + K` rule over an address range. Every region uses
/// the `scheme_entry`-style prologue, so the return-address rule is uniform
/// per target (see `image.rs`).
pub struct CfiRecord {
    pub start: Label,
    pub end: Label,
    pub cfa_offset: u32,
}

/// Target of an `RpHeader`'s mv return address.
#[derive(Clone, Copy)]
pub enum HeaderTarget {
    Absolute(usize),
    Label(Label),
}

/// A `toplink` resolved at load: the distance from the header's `toplink`
/// word back to the start of the image, which is the code chunk's header.
pub const TOPLINK_TO_IMAGE_START: usize = usize::MAX;

/// The words of a return-point header, filled at load.
pub enum HeaderWords {
    /// `rp-header`: 32 bytes.
    Full {
        mv_return_address: HeaderTarget,
        livemask: usize,
        toplink: usize,
        frame_size: isize,
    },
    /// `rp-compact-header`: 16 bytes.
    Compact(RpCompactHeader),
}

/// A return-point header reserved before its return address.
pub struct PendingHeader {
    pub header: Label,
    pub ra: Label,
    pub words: HeaderWords,
}

pub struct Masm {
    asm: Assembler,
    symbols: Vec<SymbolRecord>,
    cfi: Vec<CfiRecord>,
    headers: Vec<PendingHeader>,
    externs: Vec<ExternalName>,
}

impl Masm {
    pub fn new() -> Self {
        Self {
            asm: Assembler::native(),
            symbols: Vec::new(),
            cfi: Vec::new(),
            headers: Vec::new(),
            externs: Vec::new(),
        }
    }

    pub fn target(&self) -> Target {
        self.asm.target()
    }

    pub fn assembler(&mut self) -> &mut Assembler {
        &mut self.asm
    }

    pub fn label(&mut self) -> Label {
        self.asm.label()
    }

    pub fn bind(&mut self, label: Label) {
        self.asm.bind(label);
    }

    pub fn align(&mut self, alignment: u32) {
        self.asm.align(alignment);
    }

    /// Pad so that a patchable `jmp` emitted next has its displacement
    /// field 4-byte aligned, which keeps it inside one aligned 8-byte word:
    /// the runtime retargets it with a single atomic 4-byte store
    /// ([`CodeMemory::patch_live_u32`](crate::runtime::code_memory::CodeMemory::patch_live_u32))
    /// while other threads may execute it. AArch64 instructions are already
    /// 4-aligned and the `b` is the field. On x86-64 the rel32 ends the
    /// 5-byte `jmp rel32`, so the jump starts at 3 mod 4: a real `align`
    /// (the assembler keeps it through jump shrinking) and three 1-byte
    /// `nop`s.
    pub fn align_patchable_jump(&mut self) {
        self.asm.align(4);
        if self.asm.target() == Target::X64 {
            for _ in 0..3 {
                onlineasm!(self.asm; nop;);
            }
        }
    }

    pub fn define_symbol(
        &mut self,
        name: impl Into<String>,
        start: Label,
        end: Label,
        kind: SymbolKind,
    ) {
        self.symbols.push(SymbolRecord {
            name: name.into(),
            start,
            end,
            kind,
        });
    }

    pub fn cfi_region(&mut self, start: Label, end: Label, cfa_offset: u32) {
        self.cfi.push(CfiRecord {
            start,
            end,
            cfa_offset,
        });
    }

    /// Declares `name` as a far extern and records it once, so declaring the
    /// same name again returns the same handle.
    fn declare_extern(&mut self, name: ExternalName) -> Extern {
        let ext = self.asm.extern_far(name.clone());
        if !self.externs.contains(&name) {
            self.externs.push(name);
        }
        ext
    }

    /// Declares a Rust helper as a far extern, resolved to its address at
    /// load. Declaring the same helper again returns the same handle.
    pub fn declare_rust_fn(&mut self, helper: RuntimeExtern) -> Extern {
        self.declare_extern(helper.external())
    }

    /// Declares a direct prim wrapper by prim id. The JIT resolves it to
    /// the wrapper's address at load, declaring the same id again returns
    /// the same handle.
    pub fn declare_prim_fn(&mut self, id: u16) -> Extern {
        self.declare_extern(ExternalName::user(EXTERN_NAMESPACE_PRIM, id as u32))
    }

    /// Declares a test-only helper under the test namespace.
    #[cfg(test)]
    pub fn declare_test_fn(&mut self, index: u32) -> Extern {
        self.declare_extern(ExternalName::user(EXTERN_NAMESPACE_TEST, index))
    }

    /// Branch to `target` when `val` is an int32. `NUMBER_TAG` is exactly
    /// the top 15 bits, so `raw >= NUMBER_TAG` (unsigned) iff every tag bit
    /// is set. Reads the reserved tag register, `tmp` is unused, kept so callers need no target-specific scratch.
    pub fn branch_if_fixnum(&mut self, val: VReg, tmp: VReg, target: Label) {
        let _ = tmp;
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            bqaeq {val}, {number_tag}, {target};
        );
    }

    pub fn branch_if_not_fixnum(&mut self, val: VReg, tmp: VReg, target: Label) {
        let _ = tmp;
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            bqb {val}, {number_tag}, {target};
        );
    }

    /// Branch to `target` when `val` is a heap object (`raw & NOT_CELL_MASK
    /// == 0`, excluding the empty sentinel). Reads the reserved mask
    /// register. Clobbers `tmp`. On x86-64 the mask folds into the test
    /// branch (single `test`), elsewhere an `and` feeds the 2-operand form
    /// (the masked form would clobber `val` on AArch64).
    pub fn branch_if_cell(&mut self, val: VReg, tmp: VReg, target: Label) {
        let not_cell = regs::NOT_CELL_MASK;
        #[cfg(target_arch = "x86_64")]
        {
            let _ = tmp;
            onlineasm!(self.asm;
                bqeq {val}, 0, .not;
                btqz {val}, {not_cell}, {target};
            .not:
            );
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            onlineasm!(self.asm;
                bqeq {val}, 0, .not;
                andq {tmp}, {val}, {not_cell};
                btqz {tmp}, {target};
            .not:
            );
        }
    }

    pub fn branch_if_not_cell(&mut self, val: VReg, tmp: VReg, target: Label) {
        let not_cell = regs::NOT_CELL_MASK;
        #[cfg(target_arch = "x86_64")]
        {
            let _ = tmp;
            onlineasm!(self.asm;
                bqeq {val}, 0, {target};
                btqnz {val}, {not_cell}, {target};
            );
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            onlineasm!(self.asm;
                bqeq {val}, 0, {target};
                andq {tmp}, {val}, {not_cell};
                btqnz {tmp}, {target};
            );
        }
    }

    /// Branch to `target` when `val` is a boxed double (an inline number that
    /// is not an int32). Reads the reserved tag register. Clobbers `tmp`.
    pub fn branch_if_double(&mut self, val: VReg, tmp: VReg, target: Label) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            andq {tmp}, {val}, {number_tag};
            btqz {tmp}, .not;
            bqeq {tmp}, {number_tag}, .not;
            jmp {target};
        .not:
        );
    }

    pub fn branch_if_not_double(&mut self, val: VReg, tmp: VReg, target: Label) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            andq {tmp}, {val}, {number_tag};
            btqz {tmp}, {target};
            bqeq {tmp}, {number_tag}, {target};
        );
    }

    /// Branch to `target` when `val` is an immediate (`raw &
    /// NOT_CELL_MASK != 0`), for `immediate?`.
    /// Clobbers `tmp`, `val` is preserved.
    pub fn branch_if_immediate(&mut self, val: VReg, tmp: VReg, target: Label) {
        let not_cell = regs::NOT_CELL_MASK;
        #[cfg(target_arch = "x86_64")]
        {
            let _ = tmp;
            onlineasm!(self.asm;
                btqnz {val}, {not_cell}, {target};
            );
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            onlineasm!(self.asm;
                andq {tmp}, {val}, {not_cell};
                btqnz {tmp}, {target};
            );
        }
    }

    /// Branch to `target` when `val` is not an immediate
    pub fn branch_if_not_immediate(&mut self, val: VReg, tmp: VReg, target: Label) {
        let not_cell = regs::NOT_CELL_MASK;
        #[cfg(target_arch = "x86_64")]
        {
            let _ = tmp;
            onlineasm!(self.asm;
                btqz {val}, {not_cell}, {target};
            );
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            onlineasm!(self.asm;
                andq {tmp}, {val}, {not_cell};
                btqz {tmp}, {target};
            );
        }
    }

    /// Branch to `target` unless `val` is a heap object whose header class id
    /// is `class_id`.
    pub fn branch_not_heap_class(&mut self, val: VReg, class_id: u32, tmp: VReg, target: Label) {
        let not_cell = regs::NOT_CELL_MASK;
        let class_mask = sabi::CLASS_ID_MASK as i64;
        let want = class_id as i64;
        #[cfg(target_arch = "x86_64")]
        {
            onlineasm!(self.asm;
                bqeq {val}, 0, {target};
                btqnz {val}, {not_cell}, {target};
                loadq {tmp}, [{val} - 8];
                andq {tmp}, {tmp}, {class_mask};
                bqneq {tmp}, {want}, {target};
            );
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            onlineasm!(self.asm;
                bqeq {val}, 0, {target};
                andq {tmp}, {val}, {not_cell};
                btqnz {tmp}, {target};
                loadq {tmp}, [{val} - 8];
                andq {tmp}, {tmp}, {class_mask};
                bqneq {tmp}, {want}, {target};
            );
        }
    }

    /// `dst = box(src as i32)`. Only the low 32 bits of `src` are read, the
    /// tag comes from the reserved register.
    pub fn box_int32(&mut self, dst: VReg, src: VReg) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            sxi2q {dst}, {src};
            orq {dst}, {dst}, {number_tag};
        );
    }

    /// `dst = unbox(val) as i32`, sign-extended.
    pub fn unbox_int32(&mut self, dst: VReg, src: VReg) {
        onlineasm!(self.asm;
            sxi2q {dst}, {src};
        );
    }

    /// `dst = box(src)` where `src` holds raw `f64` bits.
    pub fn box_double_gpr(&mut self, dst: VReg, src: VReg) {
        let offset = Value::DOUBLE_ENCODE_OFFSET as i64;
        onlineasm!(self.asm;
            addq {dst}, {src}, {offset};
        );
    }

    /// `dst = unbox(val)` as raw `f64` bits.
    pub fn unbox_double_gpr(&mut self, dst: VReg, src: VReg) {
        let offset = Value::DOUBLE_ENCODE_OFFSET as i64;
        onlineasm!(self.asm;
            subq {dst}, {src}, {offset};
        );
    }

    /// `dst = box(src)` where `src` is an `f64` register.
    pub fn box_double_fpr(&mut self, dst: VReg, src: VReg) {
        let offset = Value::DOUBLE_ENCODE_OFFSET as i64;
        let nan = Value::from_f64(f64::NAN).raw_i64();
        onlineasm!(self.asm;
            bdnequn {src}, {src}, .nan;
            fd2q {dst}, {src};
            addq {dst}, {dst}, {offset};
            jmp .done;
        .nan:
            move {dst}, {nan};
        .done:
        );
    }

    /// `dst = unbox(val)` into an `f64` register. Clobbers `tmp`.
    pub fn unbox_double_fpr(&mut self, dst: VReg, src: VReg, tmp: VReg) {
        let offset = Value::DOUBLE_ENCODE_OFFSET as i64;
        onlineasm!(self.asm;
            subq {tmp}, {src}, {offset};
            fq2d {dst}, {tmp};
        );
    }

    /// `dst = src as f64` for a boxed fixnum `src` into an `f64` register.
    pub fn fixnum_to_double(&mut self, dst: VReg, src: VReg) {
        onlineasm!(self.asm;
            ci2ds {dst}, {src};
        );
    }

    /// Emits `bytes` of nops (one byte each on x64, four on fixed-width
    /// targets) as placeholder data patched after load.
    // TODO: extend onlineasm's embed_data to reserve data directly.
    fn emit_nop_padding(&mut self, bytes: usize) {
        let nops = match self.asm.target() {
            Target::X64 => bytes,
            _ => bytes / 4,
        };
        for _ in 0..nops {
            onlineasm!(self.asm; nop;);
        }
    }

    /// Reserves `bytes` of 8-aligned header before a new return address
    /// label.
    fn reserve_rp_header(&mut self, bytes: usize, ra: Label, words: HeaderWords) -> Label {
        self.asm.align(8);
        let header = self.asm.label();
        self.asm.bind(header);
        self.emit_nop_padding(bytes);
        self.asm.bind(ra);
        self.headers.push(PendingHeader { header, ra, words });
        ra
    }

    /// Emits an 8-aligned `RpHeader`
    /// (4 words) and returns the label just after it, the machine return
    /// address.
    pub fn emit_rp_header(
        &mut self,
        mv_return_address: HeaderTarget,
        livemask: usize,
        toplink: usize,
        frame_size: isize,
    ) -> Label {
        let ra = self.asm.label();
        self.reserve_rp_header(
            sabi::SIZE_RP_HEADER,
            ra,
            HeaderWords::Full {
                mv_return_address,
                livemask,
                toplink,
                frame_size,
            },
        )
    }

    /// Like [`Masm::emit_rp_header`] with the mv return address pointing at
    /// its own return address: a stub takes any value count itself.
    pub fn emit_rp_header_self(
        &mut self,
        livemask: usize,
        toplink: usize,
        frame_size: isize,
    ) -> Label {
        let ra = self.asm.label();
        self.reserve_rp_header(
            sabi::SIZE_RP_HEADER,
            ra,
            HeaderWords::Full {
                mv_return_address: HeaderTarget::Label(ra),
                livemask,
                toplink,
                frame_size,
            },
        )
    }

    /// Like [`Masm::emit_rp_header`] for the existing, unbound return
    /// address `ra`.
    pub fn emit_rp_header_at(
        &mut self,
        ra: Label,
        mv_return_address: HeaderTarget,
        livemask: usize,
        toplink: usize,
        frame_size: isize,
    ) {
        self.reserve_rp_header(
            sabi::SIZE_RP_HEADER,
            ra,
            HeaderWords::Full {
                mv_return_address,
                livemask,
                toplink,
                frame_size,
            },
        );
    }

    /// Like [`Masm::emit_rp_compact_header`] for the existing, unbound
    /// return address `ra`.
    pub fn emit_rp_compact_header_at(&mut self, ra: Label, header: RpCompactHeader) {
        self.reserve_rp_header(
            sabi::SIZE_RP_COMPACT_HEADER,
            ra,
            HeaderWords::Compact(header),
        );
    }

    /// Reserve `bytes` (a multiple of 8) of code to patch after load.
    pub fn reserve_words(&mut self, bytes: usize) {
        self.asm.align(8);
        self.emit_nop_padding(bytes);
    }

    /// Emits an 8-aligned `RpCompactHeader` (2 words) and returns the
    /// return address after it.
    pub fn emit_rp_compact_header(
        &mut self,
        toplink: usize,
        values_error: bool,
        frame_words: usize,
        livemask: usize,
    ) -> Label {
        let ra = self.asm.label();
        self.reserve_rp_header(
            sabi::SIZE_RP_COMPACT_HEADER,
            ra,
            HeaderWords::Compact(RpCompactHeader::new(
                toplink,
                values_error,
                frame_words,
                livemask,
            )),
        )
    }

    /// Jumps to the multiple-values target of the compiled return address
    /// in `ra`, or to `slow` for a runtime-owned address or a values-error
    /// header. `ra` is not the interpreter's return glue: that resumes in
    /// the caller's bytecode, which only the contexts can emit. Terminal:
    /// every arm jumps.
    /// Clobbers `tmp0`/`tmp1`.
    pub fn emit_mv_jump(&mut self, ra: VReg, tmp0: VReg, tmp1: VReg, slow: Label) {
        // `toplink` sits at `ra - 16` in both headers, 0
        // means the runtime owns the address (`underflow`, `subr_ret`, the
        // exits), which only the slow path handles.
        let toplink_at = self.asm.mem(ra, sabi::RETURN_ADDRESS_TOPLINK_DISP as i32);
        // The mode word at `ra - 8` in both: bit 0 set means a compact
        // header, and then bit 1 is the values-error bit (`sabi::mv_target`
        // reads the same words).
        let mode_at = self
            .asm
            .mem(ra, sabi::COMPACT_RETURN_ADDRESS_MASK_SIZE_MODE_DISP as i32);
        // A full header's mv return address at `ra - 32`.
        let mv_at = self
            .asm
            .mem(ra, sabi::RETURN_ADDRESS_MV_RETURN_ADDRESS_DISP as i32);

        onlineasm!(self.asm;
            loadq {tmp0}, {toplink_at};
            bqeq {tmp0}, 0, {slow};
            loadq {tmp0}, {mode_at};
            andq {tmp1}, {tmp0}, {sabi::COMPACT_HEADER_MASK as i64};
            btqnz {tmp1}, .compact;
            loadq {tmp0}, {mv_at};
            jmp {tmp0};
            .compact:
            andq {tmp1}, {tmp0}, {sabi::COMPACT_HEADER_VALUES_ERROR_MASK as i64};
            btqnz {tmp1}, {slow};
            jmp {ra};
        );
    }

    /// Saves the Scheme state (frame pointer and allocation cursor) into the thread context.
    pub fn save_scheme_state(&mut self) {
        self.save_scheme_state_below(0);
    }

    /// [`Masm::save_scheme_state`] with `extra` bytes pushed below the
    /// entry frame (an operation call's spill area), so that x86-64's
    /// native frame pointer still lands on the entry frame.
    pub fn save_scheme_state_below(&mut self, extra: usize) {
        let (tc, sfp) = (regs::TC, regs::SFP);
        let fp = self.asm.mem(tc, sabi::TC_STACK_FP as i32);
        #[cfg(target_arch = "x86_64")]
        {
            let native = self.asm.mem(
                onlineasm::regs::sp,
                (sabi::NATIVE_FP_FROM_SP + extra) as i32,
            );
            onlineasm!(self.asm;
                storep {fp}, {sfp};
                leap {sfp}, {native};
            );
        }
        #[cfg(target_arch = "aarch64")]
        {
            let _ = extra;
            let ap = regs::AP;
            let cursor = self.asm.mem(tc, sabi::TC_AP as i32);
            onlineasm!(self.asm;
                storep {fp}, {sfp};
                storep {cursor}, {ap};
            );
        }
        #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            let _ = (tc, sfp, fp);
            unsupported_target!();
        }
    }

    /// Restores the Scheme state saved by `save_scheme_state`. The frame may
    /// have moved, so `SFP` is reloaded, not swapped back.
    pub fn restore_scheme_state(&mut self) {
        let (tc, sfp) = (regs::TC, regs::SFP);
        let fp = self.asm.mem(tc, sabi::TC_STACK_FP as i32);
        #[cfg(target_arch = "x86_64")]
        onlineasm!(self.asm;
            loadp {sfp}, {fp};
        );
        #[cfg(target_arch = "aarch64")]
        {
            let ap = regs::AP;
            let cursor = self.asm.mem(tc, sabi::TC_AP as i32);
            onlineasm!(self.asm;
                loadp {sfp}, {fp};
                loadp {ap}, {cursor};
            );
        }
        #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            let _ = (tc, sfp, fp);
            unsupported_target!();
        }
    }

    /// Save Scheme state, `call` a Rust `extern "C"` function declared with
    /// [`Masm::declare_rust_fn`] (arguments already in C argument registers),
    /// restore state. The call clobbers caller-saved registers, on AArch64 it
    /// also clobbers `lr`, which frame-owning entries spill in their prologue.
    pub fn call_rust(&mut self, target: Extern) {
        self.save_scheme_state();
        onlineasm!(self.asm;
            call {target};
        );
        self.restore_scheme_state();
    }

    /// As [`Masm::call_rust`], but the target address is in `target`: for
    /// table-indexed calls (the template interpreter's cold prims), where
    /// each call site reaches a different function without a link-time
    /// reloc. Arguments already in C argument registers.
    pub fn call_rust_indirect(&mut self, target: VReg) {
        self.save_scheme_state();
        onlineasm!(self.asm;
            call {target};
        );
        self.restore_scheme_state();
    }

    /// The `scheme_entry`-style prologue: subtract the frame, save the SABI
    /// callee-saved registers (and `lr` on AArch64). Layout and CFI constants
    /// are `sabi::SCHEME_ENTRY_*`.
    pub fn scheme_entry_prologue(&mut self) {
        let frame = sabi::SCHEME_ENTRY_FRAME_BYTES as i32;
        #[cfg(target_arch = "x86_64")]
        {
            let (number_tag, constant_pointer, md, tc) =
                (regs::NUMBER_TAG, regs::CONSTANT_POINTER, regs::MD, regs::TC);
            let (not_cell, sfp) = (regs::NOT_CELL_MASK, regs::SFP);
            onlineasm!(self.asm;
                subp sp, {frame};
                storeq [sp], {number_tag};
                storeq [sp + 8], {constant_pointer};
                storeq [sp + 16], {md};
                storeq [sp + 24], {tc};
                storeq [sp + 32], {not_cell};
                storeq [sp + 48], {sfp};
            );
        }
        #[cfg(target_arch = "aarch64")]
        {
            let (tc, sfp, ap) = (regs::TC, regs::SFP, regs::AP);
            // `x22` is the template interpreter's opcode-map base (`regs::DISPATCH`), set once
            // per activation in `scheme_entry`, the entry frame preserves it
            // across nested activations.
            let x22 = regs::DISPATCH;
            let (pc, constant_pointer, md) = (regs::PC, regs::CONSTANT_POINTER, regs::MD);
            let (number_tag, not_cell) = (regs::NUMBER_TAG, regs::NOT_CELL_MASK);
            let free = onlineasm::regs::csr9;
            onlineasm!(self.asm;
                subp sp, {frame};
                storeq [sp], {tc};
                storeq [sp + 8], {sfp};
                storeq [sp + 16], {ap};
                storeq [sp + 24], {x22};
                storeq [sp + 32], {pc};
                storeq [sp + 40], {constant_pointer};
                storeq [sp + 48], {md};
                storeq [sp + 56], {number_tag};
                storeq [sp + 64], {not_cell};
                storeq [sp + 72], {free};
                storeq [sp + 80], lr;
            );
        }
        #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            let _ = frame;
            unsupported_target!();
        }
    }

    /// The matching epilogue: reload the saves, free the frame, `ret`.
    pub fn scheme_exit_epilogue(&mut self) {
        let frame = sabi::SCHEME_ENTRY_FRAME_BYTES as i32;
        #[cfg(target_arch = "x86_64")]
        {
            let (number_tag, constant_pointer, md, tc) =
                (regs::NUMBER_TAG, regs::CONSTANT_POINTER, regs::MD, regs::TC);
            let (not_cell, sfp) = (regs::NOT_CELL_MASK, regs::SFP);
            onlineasm!(self.asm;
                loadq {number_tag}, [sp];
                loadq {constant_pointer}, [sp + 8];
                loadq {md}, [sp + 16];
                loadq {tc}, [sp + 24];
                loadq {not_cell}, [sp + 32];
                loadq {sfp}, [sp + 48];
                addp sp, {frame};
                ret;
            );
        }
        #[cfg(target_arch = "aarch64")]
        {
            let (tc, sfp, ap) = (regs::TC, regs::SFP, regs::AP);
            let x22 = regs::DISPATCH;
            let (pc, constant_pointer, md) = (regs::PC, regs::CONSTANT_POINTER, regs::MD);
            let (number_tag, not_cell) = (regs::NUMBER_TAG, regs::NOT_CELL_MASK);
            let free = onlineasm::regs::csr9;
            onlineasm!(self.asm;
                loadq {tc}, [sp];
                loadq {sfp}, [sp + 8];
                loadq {ap}, [sp + 16];
                loadq {x22}, [sp + 24];
                loadq {pc}, [sp + 32];
                loadq {constant_pointer}, [sp + 40];
                loadq {md}, [sp + 48];
                loadq {number_tag}, [sp + 56];
                loadq {not_cell}, [sp + 64];
                loadq {free}, [sp + 72];
                loadq lr, [sp + 80];
                addp sp, {frame};
                ret;
            );
        }
        #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            let _ = frame;
            unsupported_target!();
        }
    }

    /// Branch to `target` when `val` is a char.
    fn branch_if_char(&mut self, val: VReg, tmp: VReg, scratch: VReg, target: Label) {
        let char_mask = Value::CHAR_MASK;
        let char_tag = Value::CHAR_TAG;

        onlineasm!(self.asm;
            andq {tmp}, {val}, {char_mask};
            bqneq {tmp}, {char_tag}, .not_char;
            move {tmp}, {val};
            urshiftq {tmp}, 16;
            andq {tmp}, {tmp}, 0xffffffff;
            move {scratch}, 0xd800;
            bqb {tmp}, {scratch}, {target};
            move {scratch}, 0x10ffff;
            bqa {tmp}, {scratch}, .not_char;
            move {scratch}, 0xdfff;
            bqa {tmp}, {scratch}, .not_char;
            .not_char:
        );
    }

    pub fn switch_int_key(
        &mut self,
        val: VReg,
        key: VReg,
        scratch: VReg,
        fallthrough: Label,
        is_char: bool,
    ) {
        if is_char {
            //let is_char = self.label();
            let char_mask = Value::CHAR_MASK;
            let char_tag = Value::CHAR_TAG;
            onlineasm!(self.asm;
                andq {scratch}, {val}, {char_mask};
                bqneq {scratch}, {char_tag}, {fallthrough};
                move {key}, {val};
                urshiftq {key}, 16;
                andq {key}, {key}, 0xffffffff;
                move {scratch}, 0xd800;
                bqb {key}, {scratch}, .is_char;
                move {scratch}, 0x10ffff;
                bqa {key}, {scratch}, {fallthrough};
                move {scratch}, 0xdfff;
                bqa {key}, {scratch}, .is_char;
                jmp {fallthrough};
                .is_char:
            );
        } else {
            self.branch_if_not_fixnum(val, scratch, fallthrough);
            onlineasm!(self.asm;
                sxi2q {key}, {val};
            );
        }
    }

    /// `switch-symbol` key prolog: type-test `val` for a symbol, loading its
    /// hash into `key`, else jump to `fallthrough`. `val` is preserved,
    /// `scratch` is clobbered. Shared by both contexts (identical emission).
    pub fn switch_sym_key(&mut self, val: VReg, key: VReg, scratch: VReg, fallthrough: Label) {
        use crate::runtime::value::Symbol;
        self.branch_not_heap_class(val, builtin_class_ids::SYMBOL, scratch, fallthrough);
        let hash_at = self
            .asm
            .mem(val, core::mem::offset_of!(Symbol<'static>, hash) as i32);
        onlineasm!(self.asm;
            loadq {key}, {hash_at};
        );
    }

    /// [`or_profile_immediate`](Self::or_profile_immediate) into the cell at `addr`.
    pub fn maybe_or_profile_immediate(
        &mut self,
        profiling: bool,
        addr: VReg,
        bits: u32,
        tmp: VReg,
    ) {
        if profiling {
            let cell = self.asm.mem(addr, 0);
            self.or_profile_immediate(cell, bits, tmp);
        }
    }

    /// OR `bits` into the `AtomicU32` at `addr`, storing only when something
    /// new was seen.
    pub fn or_profile_immediate(&mut self, addr: onlineasm::Mem, bits: u32, tmp: VReg) {
        let skip = self.label();
        let bits = bits as i64;
        onlineasm!(self.asm;
            loadi {tmp}, {addr};
            andq {tmp}, {tmp}, {bits};
            bqeq {tmp}, {bits}, {skip};
            loadi {tmp}, {addr};
            orq {tmp}, {tmp}, {bits};
            storei {addr}, {tmp};
        );
        self.bind(skip);
    }

    /// [`observe_value_profile`](Self::observe_value_profile)
    pub fn maybe_observe_value_profile(
        &mut self,
        profiling: bool,
        addr: VReg,
        val: VReg,
        kind: VReg,
        tmp: VReg,
    ) {
        if profiling {
            self.observe_value_profile(addr, val, kind, tmp);
        }
    }

    /// Merge `val` into the `ValueProfile` at `addr`.
    /// `val` and `addr` are preserved, `kind` and
    /// `tmp` are clobbered.
    pub fn observe_value_profile(&mut self, addr: VReg, val: VReg, kind: VReg, tmp: VReg) {
        use crate::runtime::interp::profile::*;
        let cell = self.asm.mem(addr, 0);
        let number_tag = regs::NUMBER_TAG;
        let (not_fixnum, flonum, heap, boolean, or_bits, first, done) = (
            self.label(),
            self.label(),
            self.label(),
            self.label(),
            self.label(),
            self.label(),
            self.label(),
        );
        self.branch_if_not_fixnum(val, tmp, not_fixnum);
        onlineasm!(self.asm;
            move {kind}, {VALUE_FIXNUM as i64};
            jmp {or_bits};
        );
        self.bind(not_fixnum);
        // Not a fixnum, so any `NUMBER_TAG` bit means a flonum.
        onlineasm!(self.asm;
            andq {tmp}, {val}, {number_tag};
            btqnz {tmp}, {flonum};
        );
        self.branch_if_cell(val, tmp, heap);
        onlineasm!(self.asm;
            andq {tmp}, {val}, -2;
            bqeq {tmp}, {crate::runtime::value::Value::VALUE_FALSE}, {boolean};
            move {kind}, {VALUE_OTHER_IMM as i64};
            jmp {or_bits};
        );
        self.bind(flonum);
        onlineasm!(self.asm;
            move {kind}, {VALUE_FLONUM as i64};
            jmp {or_bits};
        );
        self.bind(boolean);
        onlineasm!(self.asm;
            move {kind}, {VALUE_BOOL as i64};
        );
        self.bind(or_bits);
        self.or_profile_register(cell, kind, tmp, done);

        self.bind(heap);
        self.load_class_id(val, kind);
        onlineasm!(self.asm;
            loadi {tmp}, {cell};
            urshiftq {tmp}, {VALUE_CLASS_SHIFT as i64};
            bqeq {tmp}, {kind}, {done};
            bqeq {tmp}, 0, {first};
            // A different class: set the multi flag unless already set.
            loadi {tmp}, {cell};
            andq {kind}, {tmp}, {VALUE_CELL_MULTI as i64};
            bqneq {kind}, 0, {done};
            orq {tmp}, {tmp}, {VALUE_CELL_MULTI as i64};
            storei {cell}, {tmp};
            jmp {done};
        );
        self.bind(first);
        onlineasm!(self.asm;
            lshiftq {kind}, {VALUE_CLASS_SHIFT as i64};
            loadi {tmp}, {cell};
            orq {tmp}, {tmp}, {kind};
            storei {cell}, {tmp};
        );
        self.bind(done);
    }

    /// OR the bits in register `bits` into the `AtomicU32` at `addr`,
    /// storing only when something new was seen, then jump to `done`.
    fn or_profile_register(&mut self, addr: onlineasm::Mem, bits: VReg, tmp: VReg, done: Label) {
        onlineasm!(self.asm;
            loadi {tmp}, {addr};
            andq {tmp}, {tmp}, {bits};
            bqeq {tmp}, {bits}, {done};
            loadi {tmp}, {addr};
            orq {tmp}, {tmp}, {bits};
            storei {addr}, {tmp};
            jmp {done};
        );
    }

    /// `out` = the class id of the heap object `val`.
    pub fn load_class_id(&mut self, val: VReg, out: VReg) {
        let class_mask = sabi::CLASS_ID_MASK as i64;
        onlineasm!(self.asm;
            loadq {out}, [{val} - 8];
            andq {out}, {out}, {class_mask};
        );
    }

    /// Jump to `slow` unless `SFP + 8*total` fits under the overflow limit.
    pub fn check_frame_limit(&mut self, total: VReg, addr: VReg, limit: VReg, slow: Label) {
        let (tc, sfp) = (regs::TC, regs::SFP);
        let esp = self.asm.mem(tc, sabi::TC_ESP as i32);
        #[cfg(target_arch = "x86_64")]
        {
            let end = self.asm.mem_indexed(sfp, total, 8, 0);
            onlineasm!(self.asm;
                leap {addr}, {end};
            );
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            onlineasm!(self.asm;
                lshiftq {total}, 3;
                addp {addr}, {sfp}, {total};
            );
        }
        onlineasm!(self.asm;
            loadp {limit}, {esp};
            bqa {addr}, {limit}, {slow};
        );
    }

    /// The yieldpoint poll
    pub fn yieldpoint_check(&mut self, tmp: VReg, slow: Label, is_loop: bool) {
        let tc = regs::TC;
        let slot = self.asm.mem(tc, sabi::TC_YIELDPOINT as i32);
        onlineasm!(self.asm;
            loadi {tmp}, {slot};
            sxi2q {tmp}, {tmp};
        );
        if is_loop {
            onlineasm!(self.asm;
                bqgt {tmp}, 0, {slow};
            );
        } else {
            onlineasm!(self.asm;
                bqneq {tmp}, 0, {slow};
            );
        }
    }

    /// The hotness bump: increment the `CodeBlock` heat counter and jump
    /// to `slow` once it reaches `threshold`.
    pub fn hot_check(&mut self, threshold: u32, ptr: VReg, count: VReg, slow: Label) {
        let sfp = regs::SFP;
        let code_block = sabi::CLOSURE_CODE_BLOCK as i32;
        let counter = sabi::CODE_BLOCK_HOT_COUNT as i32;
        let threshold = threshold as i64;
        let closure_at = self.asm.mem(sfp, 8);
        onlineasm!(self.asm;
            loadq {ptr}, {closure_at};
        );
        let block_at = self.asm.mem(ptr, code_block);
        onlineasm!(self.asm;
            loadq {ptr}, {block_at};
        );
        let counter_at = self.asm.mem(ptr, counter);
        onlineasm!(self.asm;
            loadi {count}, {counter_at};
            addq {count}, {count}, 1;
            storei {counter_at}, {count};
            bqgteq {count}, {threshold}, {slow};
        );
    }

    /// Bump heat if the JIT is on, on reaching
    /// [threshold](crate::utils::flags::jit_threshold) jump to `slow`.
    pub fn maybe_hot_check(&mut self, ptr: VReg, count: VReg, slow: Label) {
        if let Some(threshold) = flags::jit().then_some(flags::jit_threshold() as u32) {
            self.hot_check(threshold, ptr, count, slow);
        }
    }

    /// Jump to `slow` unless `x` and `y` are both fixnums.
    pub fn check_fixnums(&mut self, x: VReg, y: VReg, tmp: VReg, slow: Label) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            andq {tmp}, {x}, {y};
            bqb {tmp}, {number_tag}, {slow};
        );
    }

    /// Jump to `target` when `x` and `y` are both fixnums
    pub fn branch_if_fixnums(&mut self, x: VReg, y: VReg, tmp: VReg, target: Label) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            andq {tmp}, {x}, {y};
            bqaeq {tmp}, {number_tag}, {target};
        );
    }

    /// Jump to `slow` unless the fixnum divisor `y` divides safely.
    pub fn check_divisor(&mut self, x: VReg, y: VReg, slow: Label) {
        let number_tag = regs::NUMBER_TAG;

        onlineasm!(self.asm;
            bqeq {y}, {number_tag}, {slow};
            bqneq {x}, {i32::MIN as i64}, .no_min_max;
            bqeq {y}, {-1i64}, {slow};
            .no_min_max:
        );
    }

    /// Divide guarded fixnums: the quotient (low 32 bits) lands in `quot`,
    /// the remainder (low 32) in `x`, `y` is preserved. Only after
    /// [`Masm::check_divisor`]: the divisor is nonzero and the pair is
    /// not `MIN/-1`, so neither target faults.
    pub fn fixnum_divmod(&mut self, quot: VReg, x: VReg, y: VReg, save: VReg) {
        let (rv, ac) = (regs::RV, regs::AC);
        onlineasm!(self.asm;
            move {save}, {ac};
            move {rv}, {x};
            cdqi;
            idivi {y};
            move {quot}, {rv};
            move {rv}, 0;
            move {x}, {ac};
            move {ac}, {save};
        );
    }

    /// `dst` = boxed `x + y` for boxed fixnums `x` and `y`, jump to `slow` on signed
    /// 32-bit overflow.
    pub fn fixnum_add_checked(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, slow: Label) {
        let _ = tmp;
        onlineasm!(self.asm;
            baddio {dst}, {x}, {y}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `x + immediate` for boxed fixnum `x` and a raw decoded
    /// immediate.
    pub fn fixnum_add_immediate_checked(
        &mut self,
        dst: VReg,
        x: VReg,
        immediate: i64,
        slow: Label,
    ) {
        onlineasm!(self.asm;
            baddio {dst}, {x}, {immediate}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `x - y`, like [`Masm::fixnum_add_checked`].
    pub fn fixnum_sub_checked(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, slow: Label) {
        let _ = tmp;
        onlineasm!(self.asm;
            bsubio {dst}, {x}, {y}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `x * y`, like [`Masm::fixnum_add_checked`].
    pub fn fixnum_mul_checked(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, slow: Label) {
        let _ = tmp;
        onlineasm!(self.asm;
            bmulio {dst}, {x}, {y}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `-x` for boxed fixnum `x`, jump to `slow` on `MIN`
    /// (which negates to a bignum). There is no checked-negate branch, so
    /// `0 - x` goes through the checked subtract. Clobbers `tmp`.
    pub fn fixnum_neg_checked(&mut self, dst: VReg, x: VReg, tmp: VReg, slow: Label) {
        onlineasm!(self.asm;
            move {tmp}, 0;
            bsubio {dst}, {tmp}, {x}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `|x|` for boxed fixnum `x`, jump to `slow` on `MIN`.
    /// Non-negative values return themselves, already boxed. Clobbers `tmp`.
    pub fn fixnum_abs_checked(&mut self, dst: VReg, x: VReg, tmp: VReg, slow: Label) {
        let done = self.label();
        onlineasm!(self.asm;
            move {tmp}, 0;
            bilt {x}, {tmp}, .neg;
            move {dst}, {x};
            jmp {done};
            .neg:
            bsubio {dst}, {tmp}, {x}, {slow};
        );
        self.box_int32(dst, dst);
        self.bind(done);
    }

    /// `dst` = boxed `x / y`.
    pub fn fixnum_div_exact(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, slow: Label) {
        self.fixnum_divmod(dst, x, y, tmp);
        // The remainder's upper half is garbage on x86: extend before the
        // full-width test.
        self.unbox_int32(x, x);
        onlineasm!(self.asm;
            bqneq {x}, 0, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed truncated `x / y`.
    pub fn fixnum_quotient(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, _slow: Label) {
        self.fixnum_divmod(dst, x, y, tmp);
        self.box_int32(dst, dst);
    }

    /// `dst` = boxed `x % y` for guarded fixnums. Call only after
    /// [`Masm::check_divisor`].
    pub fn fixnum_remainder(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, _slow: Label) {
        self.fixnum_divmod(dst, x, y, tmp);
        // The remainder landed in `x`, the quotient in `dst` is dead.
        self.box_int32(dst, x);
    }

    /// `dst` = boxed `x mod y` for guarded fixnums
    pub fn fixnum_modulo(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg, _slow: Label) {
        self.fixnum_divmod(dst, x, y, tmp);
        self.unbox_int32(x, x);
        onlineasm!(self.asm;
            bqeq {x}, 0, .box_it;
            move {tmp}, 0;
            bilt {y}, {tmp}, .rhs_neg;
            bilt {x}, {tmp}, .do_add;
            jmp .box_it;
            .rhs_neg:
            bilt {x}, {tmp}, .box_it;
            .do_add:
            addi {x}, {x}, {y};
            .box_it:
        );

        self.box_int32(dst, x);
    }

    /// A 32-bit signed comparison of boxed fixnums.
    pub fn fixnum_cmp_bool(&mut self, dst: VReg, x: VReg, y: VReg, cond: Cmp) {
        let no = Value::VALUE_FALSE;
        match cond {
            Cmp::Lt => onlineasm!(self.asm; cilt {dst}, {x}, {y};),
            Cmp::Le => onlineasm!(self.asm; cilteq {dst}, {x}, {y};),
            Cmp::Eq => onlineasm!(self.asm; cieq {dst}, {x}, {y};),
            Cmp::Ge => onlineasm!(self.asm; cigteq {dst}, {x}, {y};),
            Cmp::Gt => onlineasm!(self.asm; cigt {dst}, {x}, {y};),
        }
        onlineasm!(self.asm;
            orq {dst}, {dst}, {no};
        );
    }

    /// `dst = box(x + y)` for unboxed doubles `x` and `y`, NaN-canonicalized
    /// like [`Masm::box_double_fpr`].
    pub fn flonum_add(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            addd {tmp}, {x}, {y};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(x - y)`, like [`Masm::flonum_add`].
    pub fn flonum_sub(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            subd {tmp}, {x}, {y};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(x * y)`, like [`Masm::flonum_add`].
    pub fn flonum_mul(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            muld {tmp}, {x}, {y};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(x / y)`, like [`Masm::flonum_add`].
    pub fn flonum_div(&mut self, dst: VReg, x: VReg, y: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            divd {tmp}, {x}, {y};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(-x)` for unboxed double `x`, like [`Masm::flonum_add`].
    pub fn flonum_neg(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            negd {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(|x|)` for unboxed double `x`.
    pub fn flonum_abs(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            absd {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(sqrt(x))`
    pub fn flonum_sqrt(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            sqrtd {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(floor(x))`
    pub fn flonum_floor(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            floord {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(ceiling(x))`
    pub fn flonum_ceiling(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            ceild {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(truncate(x))`
    pub fn flonum_truncate(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            truncated {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(round(x))`
    pub fn flonum_round(&mut self, dst: VReg, x: VReg, tmp: VReg) {
        onlineasm!(self.asm;
            roundd {tmp}, {x};
        );
        self.box_double_fpr(dst, tmp);
    }

    /// `dst = box(trunc(val))`
    pub fn checked_flonum_to_fixnum(
        &mut self,
        dst: VReg,
        val: VReg,
        f0: VReg,
        f1: VReg,
        tmp: VReg,
        slow: Label,
    ) {
        self.unbox_double_fpr(f0, val, tmp);
        onlineasm!(self.asm;
            truncated2is {dst}, {f0};
            sxi2q {dst}, {dst};
            bqlt {dst}, 0, {slow};
            ci2d {f1}, {dst};
            bdneq {f1}, {f0}, {slow};
        );
        self.box_int32(dst, dst);
    }

    /// Jump to `target` when the boxed fixnum `val` is zero (boxed zero is
    /// exactly `NUMBER_TAG`). For the `quotient` leaves, whose zero cases
    /// need the thunk (fixnum zero divides to a fixnum, flonum zeros to
    /// a canonical `+0.0`).
    pub fn branch_if_zero_fixnum(&mut self, val: VReg, target: Label) {
        let number_tag = regs::NUMBER_TAG;
        onlineasm!(self.asm;
            bqeq {val}, {number_tag}, {target};
        );
    }

    /// Jump to `target` when the unboxed double `x` is either zero.
    pub fn branch_if_zero_double(&mut self, x: VReg, ftmp: VReg, gtmp: VReg, target: Label) {
        onlineasm!(self.asm;
            move {gtmp}, 0;
            fq2d {ftmp}, {gtmp};
            bdeq {x}, {ftmp}, {target};
        );
    }

    pub fn flonum_finite_bool(&mut self, dst: VReg, val: VReg, tmp: VReg) {
        let no = Value::VALUE_FALSE;
        self.unbox_double_gpr(tmp, val);
        onlineasm!(self.asm;
            urshiftq {tmp}, 52;
            andq {tmp}, {tmp}, 0x7ff;
            cqlt {dst}, {tmp}, 0x7ff;
            orq {dst}, {dst}, {no};
        );
    }

    pub fn flonum_nan_bool(&mut self, dst: VReg, val: VReg, f0: VReg, tmp: VReg) {
        let (is_nan, done) = (self.label(), self.label());
        self.unbox_double_fpr(f0, val, tmp);
        onlineasm!(self.asm;
            bdnequn {f0}, {f0}, {is_nan};
            move {dst}, {Value::VALUE_FALSE};
            jmp {done};
        );
        self.bind(is_nan);
        onlineasm!(self.asm;
            move {dst}, {Value::VALUE_TRUE};
        );
        self.bind(done);
    }

    pub fn flonum_cmp_bool(&mut self, dst: VReg, x: VReg, y: VReg, cond: Cmp) {
        let no = Value::VALUE_FALSE;
        match cond {
            Cmp::Lt => onlineasm!(self.asm; cdlt {dst}, {x}, {y};),
            Cmp::Le => onlineasm!(self.asm; cdlteq {dst}, {x}, {y};),
            Cmp::Eq => onlineasm!(self.asm; cdeq {dst}, {x}, {y};),
            Cmp::Ge => onlineasm!(self.asm; cdgteq {dst}, {x}, {y};),
            Cmp::Gt => onlineasm!(self.asm; cdgt {dst}, {x}, {y};),
        }
        onlineasm!(self.asm;
            orq {dst}, {dst}, {no};
        );
    }

    pub fn finish(self) -> Result<UnloadedImage, onlineasm::asmkit::AsmError> {
        let finished = self.asm.finish()?;
        Ok(UnloadedImage {
            finished,
            symbols: self.symbols,
            cfi: self.cfi,
            headers: self.headers,
            tables: Vec::new(),
        })
    }
}

impl Default for Masm {
    fn default() -> Self {
        Self::new()
    }
}
