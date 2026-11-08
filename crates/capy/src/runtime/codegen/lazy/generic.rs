//! Generic dispatch lazy sites.
//!
//! A call site whose first callee is a generic procedure and whose
//! `CallLinkInfo` carries a filled generic dispatch cache (`read_generic`)
//! materializes a class- and epoch-guarded entry into the cached method body,
//! skipping the generic closure's native dispatch:
//!
//! - the callee code block is the site's recorded monomorphic callee, and the
//!   closure's second free slot is the cached `GenericDescriptor`;
//! - the live descriptor epoch equals the cached epoch (and is even: an odd
//!   epoch marks a method change in progress), re-read after the descriptor
//!   fields through the cache's version seqlock;
//! - the argument count is at least the cached fill's, and the first one or
//!   two arguments are heap cells of the cached class ids;
//! - the first cached method is `NO_NEXT`, so the body takes `#f` for its
//!   next-method argument and the stub allocates no descriptor.
//!
//! The body is then entered exactly as the machine `generic_call` entry does:
//! the callee slot becomes the body, the next-method slot `#f`, the
//! arguments shift right by one, and `AC` grows by one. Any failed guard goes
//! back through the call cell, which fills the cache again and enters the
//! generic procedure normally.

use onlineasm::{Label, onlineasm};

use crate::runtime::codegen::masm::Masm;
use crate::runtime::interp::profile::{CallLinkInfo, GenericCacheEntry};
use crate::runtime::interp::stack::Ip;
use crate::runtime::objectmodel::Value;
use crate::runtime::objectmodel::class::generic::{cache_layout, method_layout};
use crate::runtime::sabi::{
    CLASS_ID_MASK, CLOSURE_CODE, CLOSURE_CODE_BLOCK, CLOSURE_FREE, HEADER_OFFSET, regs,
};

use super::super::template::tmp;
use super::{Lazy, Materialize, MaterializeCx};

/// The plan for a call site whose generic dispatch cache is usable.
pub(crate) struct GenericPlan {
    pc: usize,
    ip: Ip,
    link: usize,
    tail: bool,
    /// The cached fill this plan was accepted for, for diagnostics only
    epoch: u32,
}

impl GenericPlan {
    pub(crate) fn new(
        pc: usize,
        ip: Ip,
        link: usize,
        tail: bool,
        entry: GenericCacheEntry<'_>,
    ) -> Self {
        Self {
            pc,
            ip,
            link,
            tail,
            epoch: entry.epoch,
        }
    }

    /// Emit and install the stub.
    pub(crate) fn materialize<'gc>(&self, cx: &mut MaterializeCx<'gc>) -> Materialize {
        let mut masm = Masm::new();
        let start = masm.label();
        masm.bind(start);
        let cell = cx.cell_addr().as_usize();
        let (t0, t2, t3) = (tmp::T0, tmp::T2, tmp::T3);
        let fallback = masm.label();
        let class_ok = masm.label();
        let two_classes = masm.label();
        let frame = if self.tail { regs::SFP } else { tmp::T1 };

        // The site's recorded monomorphic callee, then the cache's version.
        let entry_block = masm.assembler().mem(regs::CP, CLOSURE_CODE_BLOCK as i32);
        // The shift below handles exactly the machine generic entry's one-
        // and two-argument cases.
        let arity_ok = masm.label();
        onlineasm!(*masm.assembler();
            bqeq {regs::AC}, 1, {arity_ok};
            bqneq {regs::AC}, 2, {fallback};
        );
        masm.bind(arity_ok);
        onlineasm!(*masm.assembler();
            loadq {t3}, {entry_block};
            move {t0}, {self.link as i64};
        );
        let callee_at = masm
            .assembler()
            .mem(t0, core::mem::offset_of!(CallLinkInfo, callee) as i32);
        let version_at = masm.assembler().mem(
            t0,
            core::mem::offset_of!(CallLinkInfo, generic_version) as i32,
        );
        let generic_at = masm
            .assembler()
            .mem(t0, core::mem::offset_of!(CallLinkInfo, generic) as i32);
        let descriptor_at = masm.assembler().mem(regs::CP, (CLOSURE_FREE + 8) as i32);
        onlineasm!(*masm.assembler();
            loadq {t2}, {callee_at};
            bqneq {t2}, {t3}, {fallback};
            loadi {regs::RV}, {version_at};
            andq {t3}, {regs::RV}, 1;
            bqneq {t3}, 0, {fallback};
            // The closure's second free slot is its generic descriptor.
            loadq {t2}, {generic_at};
            loadq {t3}, {descriptor_at};
            bqneq {t2}, {t3}, {fallback};
        );
        let cached_epoch = masm.assembler().mem(
            t0,
            core::mem::offset_of!(CallLinkInfo, generic_epoch) as i32,
        );
        let live_epoch = masm.assembler().mem(t2, cache_layout::EPOCH as i32);
        onlineasm!(*masm.assembler();
            loadi {t2}, {live_epoch};
            move {t3}, {t2};
            andq {t3}, {t3}, 1;
            bqneq {t3}, 0, {fallback};
            loadi {t3}, {cached_epoch};
            bqneq {t2}, {t3}, {fallback};
        );

        // Argument count and dispatch key.
        let dispatch_at = masm.assembler().mem(
            t0,
            core::mem::offset_of!(CallLinkInfo, generic_dispatch) as i32,
        );
        onlineasm!(*masm.assembler();
            loadi {t2}, {dispatch_at};
            move {t3}, {t2};
            urshiftq {t3}, 2;
            bqgt {t3}, {regs::AC}, {fallback};
            andq {t2}, {t2}, 3;
            bqeq {t2}, 0, {class_ok};
            move {t3}, 2;
            bieq {t2}, {t3}, {two_classes};
        );
        // One dispatch class: the first argument.
        emit_class_check(
            &mut masm,
            frame,
            16,
            core::mem::offset_of!(CallLinkInfo, generic_class_ids),
            t0,
            t2,
            t3,
            fallback,
        );
        onlineasm!(*masm.assembler(); jmp {class_ok};);
        masm.bind(two_classes);
        emit_class_check(
            &mut masm,
            frame,
            16,
            core::mem::offset_of!(CallLinkInfo, generic_class_ids),
            t0,
            t2,
            t3,
            fallback,
        );
        emit_class_check(
            &mut masm,
            frame,
            24,
            core::mem::offset_of!(CallLinkInfo, generic_class_ids) + 4,
            t0,
            t2,
            t3,
            fallback,
        );
        masm.bind(class_ok);
        // The version must still be the one the fields came from.
        let version_at = masm.assembler().mem(
            t0,
            core::mem::offset_of!(CallLinkInfo, generic_version) as i32,
        );
        onlineasm!(*masm.assembler();
            loadi {t2}, {version_at};
            bqneq {t2}, {regs::RV}, {fallback};
        );

        // The cached body.
        let methods_at = masm.assembler().mem(
            t0,
            core::mem::offset_of!(CallLinkInfo, generic_methods) as i32,
        );
        let first_method_at = masm.assembler().mem(t2, 8);
        onlineasm!(*masm.assembler();
            loadq {t2}, {methods_at};
            bqeq {t2}, 0, {fallback};
            loadq {t2}, {first_method_at};
            bqeq {t2}, 0, {fallback};
        );
        // The body must still be `NO_NEXT`
        let flags_at = masm.assembler().mem(t2, method_layout::FLAGS as i32);
        onlineasm!(*masm.assembler();
            loadb {t3}, {flags_at};
            andq {t3}, {t3}, {method_layout::NO_NEXT};
            bqeq {t3}, 0, {fallback};
        );
        let body_at = masm.assembler().mem(t2, method_layout::BODY as i32);
        onlineasm!(*masm.assembler(); loadq {t3}, {body_at};);
        masm.branch_if_not_cell(t3, t2, fallback);

        // Enter the body the way the machine generic entry does: body in the
        // callee slot, `#f` as the next-method lead, arguments shifted right.
        let one_arg = masm.label();
        let arg0 = masm.assembler().mem(frame, 16);
        let arg1 = masm.assembler().mem(frame, 24);
        let arg2 = masm.assembler().mem(frame, 32);
        let closure_slot = masm.assembler().mem(frame, 8);
        let lead_slot = masm.assembler().mem(frame, 16);
        let false_bits = Value::new(false).bits() as i64;
        onlineasm!(*masm.assembler();
            loadq {t0}, {arg0};
            bqneq {regs::AC}, 2, {one_arg};
            loadq {t2}, {arg1};
            storeq {arg2}, {t2};
        );
        masm.bind(one_arg);
        onlineasm!(*masm.assembler();
            storeq {arg1}, {t0};
            move {t0}, {false_bits};
            storeq {lead_slot}, {t0};
            storeq {closure_slot}, {t3};
            move {regs::CP}, {t3};
            addq {regs::AC}, {regs::AC}, 1;
        );
        if !self.tail {
            onlineasm!(*masm.assembler(); move {regs::SFP}, {tmp::T1};);
        }
        let block_at = masm.assembler().mem(t3, CLOSURE_CODE_BLOCK as i32);
        let code_at = masm.assembler().mem(t3, CLOSURE_CODE as i32);
        onlineasm!(*masm.assembler(); loadq {t2}, {block_at};);
        let interpreted = masm.label();
        super::super::jit::emit_jump_compiled_entry(&mut masm, t2, t0, interpreted);
        masm.bind(interpreted);
        onlineasm!(*masm.assembler(); jmp {code_at};);

        masm.bind(fallback);
        super::call::emit_cell_hit(&mut masm, self.ip, cell);

        let name = format!("generic_site:{:#x}:{}:{}", self.link, self.pc, self.epoch);
        match cx.install(masm, &name, start) {
            Ok(entry) => {
                log::info!(
                    target: crate::heap::logging::JIT_LOG_TARGET,
                    "generic_site: pc={} epoch={} stub={entry}",
                    self.pc,
                    self.epoch
                );
                Materialize::Code { again: true }
            }
            Err(err) => cx.fail(err),
        }
    }
}

/// Guard argument `index` (slot `offset` in the callee frame) as a heap cell
/// of the cached dispatch class at `class_offset`.
fn emit_class_check(
    masm: &mut Masm,
    frame: onlineasm::VReg,
    offset: i32,
    class_offset: usize,
    link: onlineasm::VReg,
    scratch: onlineasm::VReg,
    class: onlineasm::VReg,
    fallback: Label,
) {
    let arg = masm.assembler().mem(frame, offset);
    onlineasm!(*masm.assembler(); loadq {class}, {arg};);
    masm.branch_if_not_cell(class, scratch, fallback);
    let header = masm.assembler().mem(class, HEADER_OFFSET as i32);
    let want = masm.assembler().mem(link, class_offset as i32);
    onlineasm!(*masm.assembler();
        loadq {class}, {header};
        andq {class}, {class}, {CLASS_ID_MASK as i64};
        loadi {scratch}, {want};
        bqneq {scratch}, {class}, {fallback};
    );
}

impl Lazy for GenericPlan {
    const NAME: &'static str = "generic_site";

    fn pc(&self) -> usize {
        self.pc
    }

    fn ip(&self) -> Ip {
        self.ip
    }

    fn materialize<'gc>(&self, cx: &mut MaterializeCx<'gc>) -> Materialize {
        GenericPlan::materialize(self, cx)
    }
}
