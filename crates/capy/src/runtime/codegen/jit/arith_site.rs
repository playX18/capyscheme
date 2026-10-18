//! Arithmetic sites: lazily specialized numeric fast paths.

//! [`CompiledContext`](super::context::CompiledContext) over
//! [`OriginExits`](super::context::OriginExits).

use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering::Relaxed;

use bytecodegen::OperandWidth;

use super::super::emission::Emission;
use super::super::lazy::{CellState, HitCx, Lazy, Materialize, MaterializeCx};
use super::super::masm::Masm;
use super::super::template::{ArithShape, emit};
use super::super::template_image::declare_slow_paths;
use super::config::MATH_STUBS;
use super::context::{DecodedInstruction, FixedSpec, OriginExits, StubContext};
use crate::bytecode::def::OpcodeId;
use crate::runtime::interp::frame::CallFrame;
use crate::runtime::interp::slow;
use crate::runtime::interp::stack::Ip;
use crate::utils::flags;

/// One arithmetic site: the compile-time record of an instruction whose
/// slow edge is lazy, re-read when it misses.
pub(crate) struct ArithSite {
    pub(crate) pc: usize,
    pub(crate) op: OpcodeId,
    pub(crate) width: OperandWidth,
    pub(crate) ip: Ip,
    pub(crate) len: usize,
    /// The procedure's metadata area and bytecode entry
    pub(crate) md: usize,
    pub(crate) constant_pointer: usize,
    /// The profile cell the site's shape was read from, re-read when the
    /// site misses.
    pub(crate) profile_cell: usize,
    /// The slow body of the numeric jumps instead of the arithmetic ops.
    pub(crate) compare: bool,
}

unsafe impl Send for ArithSite {}
unsafe impl Sync for ArithSite {}

impl ArithSite {
    /// The site of `insn`, whose shape was read from `profile_cell`.
    pub(crate) fn new(insn: &DecodedInstruction, profile_cell: usize, compare: bool) -> Self {
        ArithSite {
            pc: insn.pc(),
            op: insn.op,
            width: insn.width,
            ip: insn.ip,
            len: insn.len,
            md: insn.md,
            constant_pointer: insn.constant_pointer,
            profile_cell,
            compare,
        }
    }

    fn profile(&self) -> u32 {
        // SAFETY: the cell is this site's live `AtomicU32` profile:
        // metadata never moves, and a relaxed load races safely with
        // executing threads (a stale shape only shapes the stub
        // suboptimally).
        unsafe { &*(self.profile_cell as *const AtomicU32) }.load(Relaxed)
    }

    fn decide(&self, from: CellState) -> StubDecision {
        // A specialized stub that misses goes full, whatever the profile
        // says now.
        if from == CellState::Specialized {
            return StubDecision::Generate {
                shape: ArithShape::Full,
                terminal: true,
            };
        }
        decide_stub(self.op, ArithShape::from_profile(self.profile()))
    }

    /// Emit and install the stub for `shape`
    fn emit<'gc>(
        &self,
        cx: &mut MaterializeCx<'gc>,
        shape: ArithShape,
        terminal: bool,
    ) -> std::io::Result<()> {
        let mut masm = Masm::new();
        let paths = declare_slow_paths(&mut masm);
        let start = masm.label();
        masm.bind(start);

        let mut emission = Emission::default();
        let mut context = StubContext::new(
            DecodedInstruction {
                op: self.op,
                width: self.width,
                ip: self.ip,
                len: self.len,
                entry: self.constant_pointer as Ip,
                constant_pointer: self.constant_pointer,
                md: self.md,
            },
            OriginExits {
                origin_table: cx.origin_table(),
            },
            FixedSpec {
                shape,
                cell: (!terminal).then(|| cx.cell_addr()),
            },
            &mut emission,
            paths,
        );
        emit::emit_template(&mut masm, &mut context, self.op);
        let name = format!(
            "{}_ool:{:#x}:{}",
            Self::NAME,
            self.constant_pointer,
            self.pc
        );
        cx.install(masm, &name, start)?;
        Ok(())
    }
}

impl Lazy for ArithSite {
    const NAME: &'static str = "math";

    fn pc(&self) -> usize {
        self.pc
    }

    fn ip(&self) -> Ip {
        self.ip
    }

    fn log_context(&self) -> String {
        format!("profile={:#x}", self.profile())
    }

    /// Run the generic operation, try to materialize a stub for the fresh
    /// profile, and resume past the instruction.
    fn hit<'gc>(&self, cx: &mut HitCx<'gc>) -> *const u8 {
        let mutator = cx.mutator();
        let (fp, ip) = cx.frame();
        // SAFETY: `fp` is the frame that hit, `ip` its instruction. The
        // cell outlives the origin image, which the running frame keeps
        // alive through its code block.
        unsafe {
            let md = CallFrame::create(fp).code_block().buffer.metadata();
            let next = if self.compare {
                slow::compare_slow_body(mutator, fp, ip, md)
            } else {
                slow::arith_slow_body(mutator, fp, ip, md)
            };
            cx.materialize(self);
            cx.resume_at(fp, next)
        }
    }

    fn materialize<'gc>(&self, cx: &mut MaterializeCx<'gc>) -> Materialize {
        let (shape, terminal) = match self.decide(cx.state()) {
            StubDecision::Sticky { reason } => return Materialize::Sticky(reason),
            StubDecision::Generate { shape, terminal } => (shape, terminal),
        };
        match self.emit(cx, shape, terminal) {
            Ok(()) => {
                if flags::jit_stats() {
                    MATH_STUBS.fetch_add(1, Relaxed);
                }
                // Specialized: its slow edge still hits this cell.
                Materialize::Code { again: !terminal }
            }
            Err(err) => cx.fail(err),
        }
    }
}

/// What the fresh state calls for.
enum StubDecision {
    /// Emit a stub of `shape`, its slow edge still hits the cell unless
    /// `terminal`.
    Generate { shape: ArithShape, terminal: bool },
    /// Generate nothing for `reason`, the site stays slow.
    Sticky { reason: &'static str },
}

/// The stub for `op` in `shape`
fn decide_stub(op: OpcodeId, shape: ArithShape) -> StubDecision {
    match shape {
        ArithShape::Generic => StubDecision::Sticky { reason: "generic" },
        // Defensive: the miss just observed both operands, so the fresh
        // word cannot be cold, if it ever is, staying slow is correct.
        ArithShape::Cold => StubDecision::Sticky { reason: "cold" },
        ArithShape::Full => StubDecision::Generate {
            shape: ArithShape::Full,
            terminal: true,
        },
        narrow => {
            let degenerate = matches!(
                (op, narrow),
                (
                    OpcodeId::OpRemainder | OpcodeId::OpModulo | OpcodeId::OpQuotient,
                    ArithShape::FlonumOnly
                ) | (OpcodeId::OpSqrt, ArithShape::FixnumOnly)
            );
            if degenerate {
                StubDecision::Sticky {
                    reason: "degenerate",
                }
            } else {
                StubDecision::Generate {
                    shape: narrow,
                    terminal: false,
                }
            }
        }
    }
}
