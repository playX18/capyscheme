//! Record accessor lazy sites.
//!
//! A call site whose recorded callee is a record accessor (a native procedure
//! whose code-block metadata carries the private `(kind, rtd, offset)`
//! descriptor) materializes an RTD-guarded direct tuple load instead of
//! entering the native accessor:
//!
//! - the callee's code block must be one of the site's recorded cases, so
//!   the stub only runs the accessor body for the closure it observed;
//! - the argument must be a `Tuple` of the accessor's RTD (read from the
//!   closure's second free slot, never baked: heap objects move);
//! - the flat field index, also read from the closure, must be inside the
//!   tuple's length word.
//!
//! Every failed guard enters the callee normally, which raises the exact
//! condition the accessor would have raised. This module is driven by
//! [`super::call::CallSite::materialize`]; it is a [`Lazy`] impl so the same
//! planner can be reused by a standalone cell later.

use onlineasm::{Label, onlineasm};

use crate::runtime::interp::profile::{CALL_IC_CASES, CallLinkInfo};
use crate::runtime::interp::stack::Ip;
use crate::runtime::sabi::{
    CLASS_ID_MASK, CLOSURE_FREE, HEADER_OFFSET, TUPLE_DATA, TUPLE_LENGTH, builtin_class_ids, regs,
};

use super::super::masm::Masm;
use super::super::template::tmp;
use super::{Lazy, Materialize, MaterializeCx};

/// The plan for a call site with at least one record-accessor callee.
/// `accessors[i]` is whether guard slot `i` (callee first, then the case
/// slots) named an accessor when the stub was materialized.
pub(crate) struct RecordPlan {
    pc: usize,
    ip: Ip,
    link: usize,
    tail: bool,
    accessors: [bool; 1 + CALL_IC_CASES],
}

impl RecordPlan {
    pub(crate) fn new(
        pc: usize,
        ip: Ip,
        link: usize,
        tail: bool,
        accessors: [bool; 1 + CALL_IC_CASES],
    ) -> Self {
        Self {
            pc,
            ip,
            link,
            tail,
            accessors,
        }
    }

    /// Emit and install the stub.
    pub(crate) fn materialize<'gc>(&self, cx: &mut MaterializeCx<'gc>) -> Materialize {
        let mut masm = Masm::new();
        let start = masm.label();
        masm.bind(start);
        let cell = cx.cell_addr().as_usize();
        let (t0, t2, t3) = (tmp::T0, tmp::T2, tmp::T3);
        let access = masm.label();
        let generic = masm.label();
        let fallback = masm.label();

        // Guard the recorded code blocks. A known accessor enters the direct
        // load, a known non-accessor the generic call entry, and an unknown
        // callee goes back through the cell so the site records it. `T3` is
        // the callee code block the template left there.
        // The edge also serves the template's non-closure fallthrough, so
        // take the callee's block from `CP` rather than trusting `T3`.
        let block_at = masm
            .assembler()
            .mem(regs::CP, crate::runtime::sabi::CLOSURE_CODE_BLOCK as i32);
        onlineasm!(*masm.assembler();
            loadq {t3}, {block_at};
            move {t0}, {self.link as i64};
        );
        let callee_at = masm
            .assembler()
            .mem(t0, core::mem::offset_of!(CallLinkInfo, callee) as i32);
        let first = if self.accessors[0] { access } else { generic };
        onlineasm!(*masm.assembler();
            loadq {t2}, {callee_at};
            bqeq {t2}, {t3}, {first};
        );
        for index in 0..CALL_IC_CASES {
            let case_at = masm.assembler().mem(
                t0,
                (core::mem::offset_of!(CallLinkInfo, cases) + index * 8) as i32,
            );
            let target = if self.accessors[index + 1] {
                access
            } else {
                generic
            };
            onlineasm!(*masm.assembler();
                loadq {t2}, {case_at};
                bqeq {t2}, {t3}, {target};
            );
        }
        onlineasm!(*masm.assembler(); jmp {fallback};);

        // The direct accessor body.
        masm.bind(access);
        self.emit_access(&mut masm, generic);

        // The generic call entry and the cell fallback.
        masm.bind(generic);
        super::call::emit_enter_callee_stub(&mut masm, self.tail);
        masm.bind(fallback);
        super::call::emit_cell_hit(&mut masm, self.ip, cell);

        let name = format!("record_site:{:#x}:{}", self.link, self.pc);
        match cx.install(masm, &name, start) {
            Ok(entry) => {
                log::info!(
                    target: crate::heap::logging::JIT_LOG_TARGET,
                    "record_site: pc={} stub={entry}",
                    self.pc
                );
                Materialize::Code { again: true }
            }
            Err(err) => cx.fail(err),
        }
    }

    /// The guarded direct load and the accessor's return.
    fn emit_access(&self, masm: &mut Masm, generic: Label) {
        let (t0, t2, t3) = (tmp::T0, tmp::T2, tmp::T3);
        // The accessor takes exactly one argument.
        onlineasm!(*masm.assembler(); bqneq {regs::AC}, 1, {generic};);
        // The callee frame: `SFP` for a tail call, the frame in `T1` for a
        // call (which is not yet `SFP`).
        let frame = if self.tail { regs::SFP } else { tmp::T1 };
        let arg = masm.assembler().mem(frame, 16);
        onlineasm!(*masm.assembler(); loadq {t0}, {arg};);
        masm.branch_if_not_cell(t0, t2, generic);
        // Tuple class.
        let header = masm.assembler().mem(t0, HEADER_OFFSET as i32);
        onlineasm!(*masm.assembler();
            loadq {t2}, {header};
            andq {t2}, {t2}, {CLASS_ID_MASK as i64};
            bqneq {t2}, {builtin_class_ids::TUPLE as i64}, {generic};
        );
        // Exact RTD identity: the tuple's first data word is its RTD, and
        // the accessor's captured RTD is its second free slot.
        let rtd_at = masm.assembler().mem(t0, TUPLE_DATA as i32);
        let captured_at = masm.assembler().mem(regs::CP, (CLOSURE_FREE + 8) as i32);
        onlineasm!(*masm.assembler();
            loadq {t2}, {rtd_at};
            loadq {t3}, {captured_at};
            bqneq {t2}, {t3}, {generic};
        );
        // The flat field index is a fixnum; its low 32 bits are the value.
        let offset_at = masm.assembler().mem(regs::CP, (CLOSURE_FREE + 16) as i32);
        let length_at = masm.assembler().mem(t0, TUPLE_LENGTH as i32);
        onlineasm!(*masm.assembler();
            loadis {t2}, {offset_at};
            bqlt {t2}, 0, {generic};
            loadq {t3}, {length_at};
            bqgteq {t2}, {t3}, {generic};
        );
        // `data[offset]`, computed without an index operand.
        onlineasm!(*masm.assembler();
            lshiftq {t2}, 3;
            addp {t2}, {t2}, {t0};
        );
        let field = masm.assembler().mem(t2, TUPLE_DATA as i32);
        onlineasm!(*masm.assembler(); loadq {t3}, {field};);
        // Return exactly as the accessor's `return` would: the value in the
        // callee frame's slot 2, `RV`, `AC = 1`, and the return address.
        if !self.tail {
            onlineasm!(*masm.assembler(); move {regs::SFP}, {tmp::T1};);
        }
        let slot2 = masm.assembler().mem(regs::SFP, 16);
        let ra = masm.assembler().mem(regs::SFP, 0);
        onlineasm!(*masm.assembler();
            storeq {slot2}, {t3};
            move {regs::RV}, {t3};
            move {regs::AC}, 1;
            loadq {t0}, {ra};
            jmp {t0};
        );
    }
}

impl Lazy for RecordPlan {
    const NAME: &'static str = "record_site";

    fn pc(&self) -> usize {
        self.pc
    }

    fn ip(&self) -> Ip {
        self.ip
    }

    fn materialize<'gc>(&self, cx: &mut MaterializeCx<'gc>) -> Materialize {
        RecordPlan::materialize(self, cx)
    }
}
