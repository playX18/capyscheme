//! The shared slow-path call both contexts leave through.

use crate::runtime::codegen::import::ImportKey;
use onlineasm::{Extern, VReg, onlineasm};

use super::super::masm::Masm;
use super::super::stubs;
use super::{SlowPath, SlowPaths};
use crate::runtime::objectmodel::Value;
use crate::runtime::sabi::{self, regs};

/// Stage the loop parking mask early: the loop helpers park the frame
/// under it, so it is staged from the slow jump on.
fn stage_loop_top_ip(masm: &mut Masm) {
    let (tc, pc) = (regs::TC, regs::PC);
    let top = masm.assembler().mem(tc, sabi::TC_STACK_TOP_IP as i32);
    onlineasm!(*masm.assembler(); storep {top}, {pc};);
}

/// Leave through a control slow path: stage `ThreadContext::ip` from `PC`, and
/// whatever `path` reads, then call it and jump where it says. The JIT
/// context loads `PC` (and, for a call, `MD`) with immediates first.
pub(crate) fn emit_control(masm: &mut Masm, paths: &SlowPaths, path: SlowPath) {
    {
        let (tc, pc, ac, rv) = (regs::TC, regs::PC, regs::AC, regs::RV);
        let rv_slot = masm.assembler().mem(tc, sabi::TC_RV as i32);
        let target = match path {
            SlowPath::HotEnter => paths.hot_enter,
            SlowPath::YieldLoop => {
                stage_loop_top_ip(masm);
                paths.yield_loop
            }
            SlowPath::HotLoop => {
                stage_loop_top_ip(masm);
                paths.hot_loop
            }
            SlowPath::Overflow => paths.overflow,
            SlowPath::Arity => paths.arity,
            SlowPath::EnterCase => paths.enter_case,
            SlowPath::CallSlow => {
                // The helper finds the link through the staged metadata area.
                let md = regs::MD;
                onlineasm!(*masm.assembler(); move {ac}, {md};);
                paths.call_slow
            }
            SlowPath::ReturnMv => {
                let undefined = Value::undefined().bits() as i64;
                onlineasm!(*masm.assembler();
                    move {rv}, {undefined};
                    storeq {rv_slot}, {rv};
                );
                paths.return_mv
            }
            SlowPath::Raise => paths.raise,
            SlowPath::RaiseKind(value, kind) => {
                onlineasm!(*masm.assembler();
                    move {rv}, {value};
                    storeq {rv_slot}, {rv};
                    move {ac}, {kind as i64};
                );
                paths.raise_kind
            }
            SlowPath::NumberArith => paths.number_arith,
            SlowPath::CompareJump => paths.compare_jump,
            SlowPath::VectorOp => paths.vector_op,
            SlowPath::VectorLength => paths.vector_length,
            SlowPath::PrimN => paths.prim_n,
            SlowPath::PrimError(kind) => {
                if kind != ac {
                    onlineasm!(*masm.assembler();
                        move {ac}, {kind};
                    );
                }
                paths.prim_error
            }
            SlowPath::GetSlot => paths.get_slot,
            SlowPath::PutSlot => paths.put_slot,
            SlowPath::RuntimeProc => paths.runtime_proc,
            SlowPath::Nop => paths.nop,
        };
        // `AC` is preserved for helpers that ignore it and staged for the
        // ones that read it (`argc`, the link metadata, the count, a kind).
        let ac_slot = masm.assembler().mem(tc, sabi::TC_AC as i32);
        let ip_slot = masm.assembler().mem(tc, sabi::TC_IP as i32);
        onlineasm!(*masm.assembler();
            storeq {ac_slot}, {ac};
            storep {ip_slot}, {pc};
        );
        masm.save_scheme_state();
        let (a0, _, _) = stubs::arg_regs();
        onlineasm!(*masm.assembler();
            move {a0}, {tc};
            call {target};
        );
        masm.restore_scheme_state();
        stubs::emit_control_tail(masm);
    }
}

/// Where a stub slow block finds its site address: the origin image's
/// slow block inherits it in a register from its slow edge, while a
/// lazily generated stub bakes it as an immediate.
#[derive(Clone, Copy)]
pub(crate) enum SiteOperand {
    Register(VReg),
    Cell(usize),
}

/// Leave through a stub repatching slow path: exactly [`emit_control`]'s
/// staging (the caller staged `PC`, and `AC` carries whatever the template
/// left), plus the site address in the second C argument. Terminal: calls
/// and jumps where the helper says.
pub(crate) fn emit_site_slow_call(masm: &mut Masm, target: Extern, site: SiteOperand) {
    let (tc, ac) = (regs::TC, regs::AC);
    let ac_slot = masm.assembler().mem(tc, sabi::TC_AC as i32);
    let ip_slot = masm.assembler().mem(tc, sabi::TC_IP as i32);
    let pc = regs::PC;
    onlineasm!(*masm.assembler();
        storeq {ac_slot}, {ac};
        storep {ip_slot}, {pc};
    );
    masm.save_scheme_state();
    let (a0, a1, _) = stubs::arg_regs();
    match site {
        SiteOperand::Register(register) => onlineasm!(*masm.assembler();
            move {a1}, {register};
        ),
        SiteOperand::Cell(index) => masm.load_import(a1, ImportKey::Cell(index as u32)),
    }
    onlineasm!(*masm.assembler();
        move {a0}, {tc};
        call {target};
    );
    masm.restore_scheme_state();
    stubs::emit_control_tail(masm);
}

/// Leave through `path`, staging the interpreter's `PC` (and `MD` for
/// the call link) from the captured instruction values.
pub(crate) fn emit_staged_control(
    masm: &mut Masm,
    paths: &SlowPaths,
    path: SlowPath,
    ip: crate::runtime::interp::stack::Ip,
    md: usize,
) {
    // Neither is live in compiled code otherwise.
    let (pc, m) = (regs::PC, regs::MD);
    masm.load_ip(pc, ip as usize);
    if let SlowPath::CallSlow = path {
        masm.load_md(m, md);
    }
    emit_control(masm, paths, path);
}
