//! Lazy call sites: a baseline call's slow edge materializes a guarded
//! direct-entry stub the first time it misses.
//!
//! With lazy optimizations, the baseline
//! call templates stop emitting their inline mono/poly condition and jump
//! every call to this lazy edge (`lazy_call_edge`). The first call runs the
//! existing miss path (`control_enter` with the site's `CallLinkInfo`),
//! records the callee in the site's case array, and materializes a small
//! stub:
//!
//! - a guarded dispatch over the site's recorded code blocks, entering each
//!   callee's compiled entry directly (`emit_jump_compiled_entry`), or its
//!   `closure.code` when it has none. An unknown callee jumps back into the
//!   cell's hit block, which records it and grows the stub;
//! - when a recorded callee is a record accessor, an RTD-guarded direct tuple load instead of the
//!   native accessor call ([`super::record`]);
//! - when the site's generic dispatch cache is filled and the first callee
//!   is a generic procedure, a class/epoch-guarded method-body entry
//!   ([`super::generic`]).

use std::sync::atomic::{AtomicU8, AtomicU64, Ordering::Relaxed};

use crate::heap::Gc;
use crate::runtime::MutationContext;
use crate::runtime::codegen::masm::Masm;
use crate::runtime::codegen::stubs;
use crate::runtime::interp::profile::{CALL_IC_CASES, CallLinkInfo, GenericCacheEntry};
use crate::runtime::interp::slow;
use crate::runtime::interp::stack::Ip;
use crate::runtime::objectmodel::class::generic::GenericDescriptor;
use crate::runtime::objectmodel::{Closure, CodeBlock, Value};
use crate::runtime::record_ops::{self, RECORD_KIND_ACCESSOR};
use crate::runtime::sabi::regs;

use super::super::template::SiteOperand;
use super::{CellState, HitCx, Lazy, Materialize, MaterializeCx};

/// One baseline call site's lazy cell.
pub(crate) struct CallSite {
    pub(crate) pc: usize,
    /// The call instruction's bytecode address
    pub(crate) ip: Ip,
    /// The site's `CallLinkInfo`,
    pub(crate) link: usize,
    /// Callee frame offset in slots from `SFP` (0 for tail calls).
    pub(crate) base: usize,
    pub(crate) tail: bool,
    /// The plan and case count last materialized, to avoid regenerating an
    /// identical stub on every hit.
    sig: AtomicU64,
    /// Whether the last observed callee kept its generic descriptor in the
    /// closure's second free slot (the only location the generic stub can
    /// guard without a metadata walk).
    generic_free: AtomicU8,
}

impl CallSite {
    pub(crate) fn new(pc: usize, ip: Ip, link: usize, base: usize, tail: bool) -> Self {
        Self {
            pc,
            ip,
            link,
            base,
            tail,
            sig: AtomicU64::new(0),
            generic_free: AtomicU8::new(0),
        }
    }

    fn link(&self) -> &CallLinkInfo {
        // SAFETY: the link lives in metadata of a code buffer that is alive
        // for as long as this cell (the cell is owned by the same unit and
        // the buffer keeps the unit alive).
        unsafe { &*(self.link as *const CallLinkInfo) }
    }
}

impl Lazy for CallSite {
    const NAME: &'static str = "call_site";

    fn pc(&self) -> usize {
        self.pc
    }

    fn ip(&self) -> Ip {
        self.ip
    }

    fn log_context(&self) -> String {
        let link = self.link();
        let known = known_blocks(link).len();
        format!("link={:#x} known={known}", self.link)
    }

    /// Run call miss path (which records the callee), then
    /// materialize the stub the fresh site calls for.
    fn hit<'gc>(&self, cx: &mut HitCx<'gc>) -> *const u8 {
        let mutator = cx.mutator();
        let (fp, _ip) = cx.frame();
        // The hit block staged `AC`; the frame is the caller's `SFP`.
        let argc = mutator.tc().ac.get() as usize;

        // SAFETY: the template's `base` operand is the callee frame's slot
        // offset, inside the live stack.
        let frame = if self.tail {
            fp
        } else {
            unsafe { fp.add(self.base) }
        };
        let link = self.link();

        // Record the observed callee before the miss so `materialize` sees a
        // complete case set. The miss path still records `link.callee`.
        // SAFETY: `frame[1]` is the callee closure slot of a call frame.
        let callee = unsafe { *frame.add(1) };
        if callee.is::<Closure>() {
            let closure = Closure::from_value(callee);
            let block = Value::from_gc(Closure::code_block(closure));
            stubs::note_call_case(mutator, link, block);
            // Only a descriptor in the closure's second free slot can be
            // guarded by the stub without walking the metadata alist.
            let descriptor = (closure.nfree > 1)
                .then(|| closure[1].get())
                .filter(|value| value.is::<GenericDescriptor>())
                .map(|value| value.downcast::<GenericDescriptor>());
            self.generic_free.store(descriptor.is_some() as u8, Relaxed);
            if let Some(generic) = descriptor {
                fill_generic_hint(mutator, link, frame, argc, generic);
            }
        } else {
            self.generic_free.store(0, Relaxed);
        }
        // SAFETY: forwarded from the hit; the frame and link are the live
        // call site's, and the slot lifetime is the call's.
        let next = unsafe {
            stubs::capy_ic_miss_call(
                mutator.as_ptr(),
                frame as crate::runtime::interp::stack::Slot<'static>,
                link,
            )
        };
        cx.materialize(self);
        next
    }

    fn materialize<'gc>(&self, cx: &mut MaterializeCx<'gc>) -> Materialize {
        let link = self.link();
        let plan = self.plan(link);
        let known = known_blocks(link).len() as u64;
        // The megamorphic bit changes the stub's miss arm, so a site that
        // fills its cases regenerates.
        let sig = plan.tag() | (known << 8) | ((link.is_megamorphic() as u64) << 40);
        if cx.state() == CellState::Specialized && self.sig.load(Relaxed) == sig {
            return Materialize::Later;
        }
        let outcome = match plan {
            Plan::Plain => self.emit_plain(cx, link),
            Plan::Record(plan) => plan.materialize(cx),
            Plan::Generic(plan) => plan.materialize(cx),
        };
        if matches!(outcome, Materialize::Code { .. }) {
            self.sig.store(sig, Relaxed);
        }
        outcome
    }
}

impl CallSite {
    /// What the fresh site calls for.
    fn plan(&self, link: &CallLinkInfo) -> Plan {
        // A generic site is only specialized while it is monomorphic: the
        // cached dispatch describes the first callee, and a second callee
        // makes the guarded method entry useless.
        if self.generic_free.load(Relaxed) == 1
            && known_blocks(link).len() <= 1
            && let Some(entry) = usable_generic(link)
        {
            return Plan::Generic(super::generic::GenericPlan::new(
                self.pc, self.ip, self.link, self.tail, entry,
            ));
        }
        let accessors = record_blocks(link);
        if accessors.iter().any(|accessor| *accessor) {
            return Plan::Record(super::record::RecordPlan::new(
                self.pc, self.ip, self.link, self.tail, accessors,
            ));
        }
        Plan::Plain
    }

    fn emit_plain<'gc>(&self, cx: &mut MaterializeCx<'gc>, link: &CallLinkInfo) -> Materialize {
        let mut masm = Masm::new();
        let start = masm.label();
        masm.bind(start);
        emit_plain_stub(
            &mut masm,
            link,
            self.ip,
            cx.cell_addr().as_usize(),
            !self.tail,
        );
        let name = format!("call_site:{:#x}:{}", self.link, self.pc);
        match cx.install(masm, &name, start) {
            // A miss in the stub re-enters the cell: the site can still grow.
            Ok(_) => Materialize::Code { again: true },
            Err(err) => cx.fail(err),
        }
    }
}

/// The plan a call site's fresh state calls for.
enum Plan {
    Plain,
    Record(super::record::RecordPlan),
    Generic(super::generic::GenericPlan),
}

impl Plan {
    fn tag(&self) -> u64 {
        match self {
            Plan::Plain => 1,
            Plan::Record(_) => 2,
            Plan::Generic(_) => 3,
        }
    }
}

/// The callee code blocks the site has recorded: the monomorphic `callee`
/// first, then the case slots.
fn known_blocks<'gc>(link: &'gc CallLinkInfo) -> Vec<Value<'gc>> {
    let mut blocks = Vec::with_capacity(1 + CALL_IC_CASES);
    if let Some(callee) = link.callee.get_value() {
        blocks.push(callee);
    }
    for case in &link.cases {
        if let Some(block) = case.get_value() {
            if !blocks.iter().any(|known| known.bits() == block.bits()) {
                blocks.push(block);
            }
        }
    }
    blocks
}

fn record_blocks(link: &CallLinkInfo) -> [bool; 1 + CALL_IC_CASES] {
    let mut accessor = [false; 1 + CALL_IC_CASES];
    let mut classify = |value: Option<Value<'_>>| {
        let Some(value) = value else {
            return false;
        };
        if !value.is::<CodeBlock>() {
            return false;
        }
        let block = value.downcast::<CodeBlock>();
        record_ops::record_op_from_meta(block.metadata.get())
            .is_some_and(|op| op.kind == RECORD_KIND_ACCESSOR)
    };
    accessor[0] = classify(link.callee.get_value());
    for (index, case) in link.cases.iter().enumerate() {
        accessor[index + 1] = classify(case.get_value());
    }
    accessor
}

fn usable_generic<'gc>(link: &'gc CallLinkInfo) -> Option<GenericCacheEntry<'gc>> {
    let entry = link.read_generic()?;
    if entry.key_len > 2 || entry.min_argc > 2 {
        return None;
    }
    if entry.epoch & 1 != 0 || entry.epoch != entry.generic.epoch() {
        return None;
    }
    let methods = entry.methods;
    if methods.is_empty() {
        return None;
    }
    let first = methods[0];
    if !first.flags().no_next() || !first.body().is::<Closure>() {
        return None;
    }
    Some(entry)
}

fn fill_generic_hint<'gc>(
    mutator: MutationContext<'gc>,
    link: &'gc CallLinkInfo,
    frame: *mut Value<'gc>,
    argc: usize,
    generic: Gc<'gc, GenericDescriptor<'gc>>,
) {
    let Some(buffer) = link.buffer().map(Value::from_gc) else {
        return;
    };
    // SAFETY: the frame is the live call frame and the arguments are its
    // `[2..]` slots.
    let args = unsafe { std::slice::from_raw_parts(frame.add(2), argc) };
    let epoch = generic.epoch();
    let methods = match GenericDescriptor::cached_for_args(generic, args) {
        Some(methods) => methods,
        // The dispatch may allocate while it reads the frame slice; this is
        // the same shape the interpreter's call path uses.
        None => match GenericDescriptor::applicable_methods_at(mutator, generic, args, epoch) {
            Ok(methods) => methods,
            Err(_) => return,
        },
    };
    if methods.is_empty() {
        return;
    }
    slow::fill_generic_cache(mutator, link, buffer, generic, methods, args);
}

fn emit_plain_stub(masm: &mut Masm, link: &CallLinkInfo, ip: Ip, cell: usize, is_call: bool) {
    use super::super::template::tmp;
    use crate::runtime::interp::profile::CALL_IC_CASES;
    use crate::runtime::sabi::CLOSURE_CODE;
    use onlineasm::onlineasm;

    let link_is_mega = link.is_megamorphic();
    let link = crate::runtime::codegen::addr::imm(mmtk::util::Address::from_ref(link));
    let (cp, ac, sfp) = (regs::CP, regs::AC, regs::SFP);
    let (t0, t1, t2, t3) = (tmp::T0, tmp::T1, tmp::T2, tmp::T3);
    let hit = masm.label();
    let cases = core::mem::offset_of!(CallLinkInfo, cases);
    let case_hits = core::mem::offset_of!(CallLinkInfo, case_hits);
    let hits = core::mem::offset_of!(CallLinkInfo, hits);
    let entered: Vec<_> = (0..CALL_IC_CASES).map(|_| masm.label()).collect();
    // The edge also serves the template's non-closure fallthrough, where
    // `T3` was never loaded; take the callee's block from `CP` instead (a
    // non-closure callee leaves the caller's valid `CP` here).
    let block_at = masm
        .assembler()
        .mem(cp, crate::runtime::sabi::CLOSURE_CODE_BLOCK as i32);
    onlineasm!(*masm.assembler();
        loadq {t3}, {block_at};
        move {t0}, {link};
    );
    for (index, &entered) in entered.iter().enumerate() {
        let slot = masm.assembler().mem(t0, (cases + index * 8) as i32);
        onlineasm!(*masm.assembler();
            loadq {t2}, {slot};
            bqeq {t2}, {t3}, {entered};
        );
    }
    // A megamorphic site has nothing left to record: a closure callee enters
    // through its own compiled entry without a Rust round trip. A
    // non-closure callee still goes through the cell, which owns that
    // dispatch (and left `CP` at the caller's closure).
    if link_is_mega {
        use crate::runtime::sabi::builtin_class_ids;
        let callee_at = if is_call {
            masm.assembler().mem(t1, 8)
        } else {
            masm.assembler().mem(sfp, 8)
        };
        let not_closure = masm.label();
        onlineasm!(*masm.assembler(); loadq {t2}, {callee_at};);
        masm.branch_not_heap_class(t2, builtin_class_ids::CLOSURE, t0, not_closure);
        onlineasm!(*masm.assembler();
            move {cp}, {t2};
            loadq {t3}, {block_at};
            jmp {hit};
        );
        masm.bind(not_closure);
    }
    // An unknown callee: stage the hit and let the cell record it.
    emit_cell_hit(masm, ip, cell);

    for (index, entered) in entered.into_iter().enumerate() {
        masm.bind(entered);

        stubs::emit_bump_counter(masm, t0, case_hits + index * 4, t2);
        stubs::emit_bump_counter(masm, t0, hits, t2);

        onlineasm!(*masm.assembler(); jmp {hit};);
    }
    masm.bind(hit);
    if is_call {
        onlineasm!(*masm.assembler(); move {sfp}, {t1};);
    }
    let generic = masm.label();
    let code_at = masm.assembler().mem(cp, CLOSURE_CODE as i32);
    super::super::jit::emit_jump_compiled_entry(masm, t3, t2, generic);
    masm.bind(generic);
    onlineasm!(*masm.assembler(); jmp {code_at};);
    let _ = ac;
}

pub(crate) fn emit_cell_hit(masm: &mut Masm, ip: Ip, cell: usize) {
    super::edge::emit_hit_block(masm, ip, SiteOperand::Immediate(cell));
}

pub(crate) fn emit_enter_callee_stub(masm: &mut Masm, tail: bool) {
    use crate::runtime::sabi::{CLOSURE_CODE, CLOSURE_CODE_BLOCK, regs};
    use onlineasm::onlineasm;

    use super::super::template::tmp;

    if !tail {
        onlineasm!(*masm.assembler(); move {regs::SFP}, {tmp::T1};);
    }
    // The block is reloaded from the closure: the guard chain clobbers the
    // register the template left it in.
    let block = masm.assembler().mem(regs::CP, CLOSURE_CODE_BLOCK as i32);
    onlineasm!(*masm.assembler(); loadq {tmp::T3}, {block};);
    let interpreted = masm.label();
    super::super::jit::emit_jump_compiled_entry(masm, tmp::T3, tmp::T2, interpreted);
    masm.bind(interpreted);
    let code = masm.assembler().mem(regs::CP, CLOSURE_CODE as i32);
    onlineasm!(*masm.assembler(); jmp {code};);
}
