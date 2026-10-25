//! Generated glues, stubs, and their Rust control slow paths.

use parking_lot::Mutex;
use std::io;
use std::sync::atomic::Ordering::{Acquire, Relaxed, Release};
use std::sync::atomic::{AtomicU64, AtomicUsize};

use onlineasm::{Extern, Label, VReg, onlineasm};

use super::addr::imm;
use super::gc::GcCodegen;
use super::image::LoadedImage;
use super::lazy::builder::StubBuilder;
use super::masm::{Masm, RuntimeExtern, SymbolKind};
use super::template::tmp;
use super::unit::Native;
use crate::bytecode::def;
use crate::heap::Gc;
use crate::heap::Write;
use crate::heap::mmtk::util::Address;
use crate::runtime::interp::profile::{CALL_IC_CASES, CallLinkInfo, GcCell, LINK_MEGA};
use crate::runtime::interp::rust_interp;
use crate::runtime::interp::slow;
use crate::runtime::interp::stack::Slot;
use crate::runtime::sabi::{self, regs};
use crate::runtime::value::{Closure, CodeBuffer, NativeReturn, ReturnCode, Value};
use crate::runtime::value::{CodeBlock, CodeImage};
use crate::runtime::{MutationContext, Tc};
use crate::utils::flags;

/// Dynamic bail counts per opcode byte
static BAIL_COUNTS: [AtomicU64; 256] = [const { AtomicU64::new(0) }; 256];

/// Snapshot of the per-opcode bail counts, indexed by opcode byte.
pub fn bail_counts() -> [u64; 256] {
    std::array::from_fn(|i| BAIL_COUNTS[i].load(Relaxed))
}

/// Print the top bailing opcodes to stderr. Installed with `atexit`
extern "C" fn print_bail_stats() {
    let counts = bail_counts();
    let total: u64 = counts.iter().sum();
    let mut ranked: Vec<(u8, u64)> = counts
        .iter()
        .enumerate()
        .filter(|(_, count)| **count > 0)
        .map(|(op, count)| (op as u8, *count))
        .collect();
    ranked.sort_by(|a, b| b.1.cmp(&a.1));
    log::info!(
        target: crate::heap::logging::STATS_LOG_TARGET,
        "template bail stats: {total} total"
    );
    for (op, count) in ranked.iter().take(10) {
        log::info!(
            target: crate::heap::logging::STATS_LOG_TARGET,
            "  {}: {count}",
            super::template::opcode_name(*op)
        );
    }
}

unsafe extern "C" {
    fn atexit(callback: extern "C" fn());
}

pub fn maybe_report_bail_stats() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    if !flags::template_bail_stats() {
        return;
    }
    ONCE.call_once(|| {
        // SAFETY: the callback only reads atomics and logs.
        unsafe { atexit(print_bail_stats) };
    });
}

/// Executed-opcode counts on the template interpreter
static OP_COUNTS: [AtomicU64; 3 * 256] = [const { AtomicU64::new(0) }; 3 * 256];

/// Snapshot of the executed-opcode counts, indexed by `[width][byte]`.
pub fn op_counts() -> [[u64; 256]; 3] {
    std::array::from_fn(|width| {
        std::array::from_fn(|byte| OP_COUNTS[width * 256 + byte].load(Relaxed))
    })
}

fn op_count_addr(width: usize, byte: u8) -> usize {
    debug_assert!(width < 3);
    &OP_COUNTS[width * 256 + byte as usize] as *const AtomicU64 as usize
}

/// Generator flag for template-interpreter executed-opcode counting.
pub fn op_stats_enabled() -> bool {
    flags::template_op_stats()
}

/// Print executed-opcode totals to stderr
extern "C" fn print_op_stats() {
    let counts = op_counts();
    let width_totals: [u64; 3] = std::array::from_fn(|width| counts[width].iter().sum());
    let total: u64 = width_totals.iter().sum();
    log::info!(
        target: crate::heap::logging::STATS_LOG_TARGET,
        "template op stats: {total} total ({} narrow, {} wide16, {} wide32)",
        width_totals[0], width_totals[1], width_totals[2]
    );
    let mut ranked: Vec<(usize, u8, u64)> = Vec::new();
    for (width, table) in counts.iter().enumerate() {
        for (byte, count) in table.iter().enumerate() {
            if *count > 0 {
                ranked.push((width, byte as u8, *count));
            }
        }
    }
    ranked.sort_by(|a, b| b.2.cmp(&a.2));
    for (width, op, count) in ranked.iter().take(15) {
        let width_name = ["narrow", "wide16", "wide32"][*width];
        let name: &str = match (*width, *op) {
            (0, b) if b == def::WIDE16_PREFIX => "op_wide16",
            (0, b) if b == def::WIDE32_PREFIX => "op_wide32",
            _ => super::template::opcode_name(*op),
        };
        log::info!(
            target: crate::heap::logging::STATS_LOG_TARGET,
            "  {name}.{width_name}: {count}"
        );
    }
}

/// Install the `CAPY_TEMPLATE_OP_STATS=1` exit report, once per process.
pub fn maybe_report_op_stats() {
    use std::sync::Once;
    static ONCE: Once = Once::new();
    if !op_stats_enabled() {
        return;
    }
    ONCE.call_once(|| {
        // SAFETY: the callback only reads atomics and logs.
        unsafe { atexit(print_op_stats) };
    });
}

static INTERPRETER_RESUME: AtomicUsize = AtomicUsize::new(0);
static EXIT_GLUE: AtomicUsize = AtomicUsize::new(0);

pub fn set_exit_glue(addr: *const u8) {
    EXIT_GLUE.store(addr as usize, Relaxed);
}

pub(crate) fn exit_glue_addr() -> *const u8 {
    EXIT_GLUE.load(Relaxed) as *const u8
}

pub fn set_interpreter_resume(addr: *const u8) {
    INTERPRETER_RESUME.store(addr as usize, Relaxed);
}

pub(crate) fn interpreter_resume_addr() -> *const u8 {
    let addr = INTERPRETER_RESUME.load(Relaxed);
    debug_assert!(
        addr != 0,
        "slow path ran before the template interpreter installed interpreter_resume"
    );
    addr as *const u8
}

/// Whether `capy_bail` keeps the one-way behavior.
fn bail_oneway() -> bool {
    flags::template_bail_oneway()
}

/// Execute the bailing instruction in the Rust engine, then resume the template interpreter.
pub unsafe extern "C" fn capy_bail(th: *const (), op: u64, _width: u64) -> *const u8 {
    unsafe {
        BAIL_COUNTS[(op as u8) as usize].fetch_add(1, Relaxed);

        let mutator = MutationContext::from_ptr(th);
        let tc: &Tc<'static> = mutator.tc();
        let fp = tc.stack.fp.get();
        let ip = tc.ip.get();
        let w = tc.ac.get();
        if bail_oneway() {
            let ret: NativeReturn<'static> = rust_interp::resume_at(mutator, ip, fp, w);
            tc.ac.set(ret.code as usize as u64);
            tc.rv.set(ret.value);
            tc.cp.set(Value::undefined());
            tc.ip.set(std::ptr::null());
            return exit_glue_addr();
        }
        let ret: NativeReturn<'static> = rust_interp::resume_one(mutator, ip, fp, w);
        match ret.code {
            ReturnCode::ResumeTemplate => interpreter_resume_addr(),
            ReturnCode::ReturnOk | ReturnCode::ReturnErr => {
                tc.ac.set(ret.code as usize as u64);
                tc.rv.set(ret.value);
                tc.cp.set(Value::undefined());
                tc.ip.set(std::ptr::null());
                exit_glue_addr()
            }
            code => unreachable!("capy_bail: engine returned {code:?}"),
        }
    }
}

/// C argument registers for `(th, fp, argc)`.
pub fn arg_regs() -> (VReg, VReg, VReg) {
    #[cfg(target_arch = "x86_64")]
    {
        use onlineasm::regs::{t1 as a1, t2 as a2, t6 as a0};
        (a0, a1, a2)
    }
    #[cfg(target_arch = "aarch64")]
    {
        use onlineasm::regs::{t0 as a0, t1 as a1, t2 as a2};
        (a0, a1, a2)
    }
    #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
    {
        unsupported_target!()
    }
}

/// The first five C argument registers
pub(crate) fn arg_regs5() -> (VReg, VReg, VReg, VReg, VReg) {
    #[cfg(target_arch = "x86_64")]
    {
        use onlineasm::regs::{t1 as a1, t2 as a2, t3 as a3, t4 as a4, t6 as a0};
        (a0, a1, a2, a3, a4)
    }
    #[cfg(target_arch = "aarch64")]
    {
        use onlineasm::regs::{t0 as a0, t1 as a1, t2 as a2, t3 as a3, t4 as a4};
        (a0, a1, a2, a3, a4)
    }
    #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
    {
        unsupported_target!()
    }
}

/// C return registers for a two-word `#[repr(C)]` return:
/// `NativeReturn(code, value)` and the direct prim result (value, error).
pub(crate) fn ret_regs() -> (VReg, VReg) {
    #[cfg(target_arch = "x86_64")]
    {
        use onlineasm::regs::{t0 as r0, t2 as r1};
        (r0, r1)
    }
    #[cfg(target_arch = "aarch64")]
    {
        use onlineasm::regs::{t0 as r0, t1 as r1};
        (r0, r1)
    }
    #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
    {
        unsupported_target!()
    }
}

/// Narrow dispatch through the opcode map at `map`: `jump map[byte at PC]`.
/// On AArch64 the map base lives in [`regs::DISPATCH`](crate::runtime::sabi::regs)
/// (set once in [`emit_scheme_entry`]).
pub fn emit_dispatch(masm: &mut Masm, map: Label) {
    let pc = regs::PC;
    #[cfg(target_arch = "aarch64")]
    {
        let t5 = tmp::T2;
        let slot = masm.assembler().mem_indexed(regs::DISPATCH, t5, 8, 0);
        onlineasm!(*masm.assembler();
            loadb {t5}, [{pc}];
            jmp {slot};
        );
        let _ = map;
    }
    #[cfg(not(target_arch = "aarch64"))]
    {
        let (t5, t7) = (tmp::T2, tmp::T3);
        let slot = masm.assembler().mem_indexed(t7, t5, 8, 0);
        onlineasm!(*masm.assembler();
            loadb {t5}, [{pc}];
            leap {t7}, {map};
            jmp {slot};
        );
    }
}

/// Dispatch when the next opcode byte is already in the dispatch scratch
/// (`t5`): the table jump without the opcode load. Used by fall-through
/// `next`, which issues the load before bumping `PC`.
pub fn emit_dispatch_preloaded(masm: &mut Masm, map: Label) {
    #[cfg(target_arch = "aarch64")]
    {
        let t5 = tmp::T2;
        let slot = masm.assembler().mem_indexed(regs::DISPATCH, t5, 8, 0);
        onlineasm!(*masm.assembler();
            jmp {slot};
        );
        let _ = map;
    }
    #[cfg(not(target_arch = "aarch64"))]
    {
        let (t5, t7) = (tmp::T2, tmp::T3);
        let slot = masm.assembler().mem_indexed(t7, t5, 8, 0);
        onlineasm!(*masm.assembler();
            leap {t7}, {map};
            jmp {slot};
        );
    }
}

pub fn emit_op_count(masm: &mut Masm, width: usize, byte: u8, enabled: bool) {
    if !enabled {
        return;
    }
    let addr = op_count_addr(width, byte) as i64;
    let (t0, t1) = (tmp::T0, tmp::T1);
    onlineasm!(*masm.assembler();
        move {t1}, {addr};
        loadq {t0}, [{t1}];
        addq {t0}, {t0}, 1;
        storeq [{t1}], {t0};
    );
}

/// Reload the SABI transient registers from the thread context.
fn reload_transient(masm: &mut Masm) {
    let tc = regs::TC;
    let ac = masm.assembler().mem(tc, sabi::TC_AC as i32);
    let rv = masm.assembler().mem(tc, sabi::TC_RV as i32);
    let cp = masm.assembler().mem(tc, sabi::TC_CP as i32);
    let (ac_register, rv_register, cp_register) = (regs::AC, regs::RV, regs::CP);
    onlineasm!(*masm.assembler();
        loadq {ac_register}, {ac};
        loadq {rv_register}, {rv};
        loadq {cp_register}, {cp};
    );
}

/// Tail of a stub or bail handler: the control path returned its address in
/// `r0`, save it aside, reload the transient registers, and jump there.
pub(crate) fn emit_control_tail(masm: &mut Masm) {
    let (r0, _) = ret_regs();
    let scratch = tmp::T2;
    onlineasm!(*masm.assembler();
        move {scratch}, {r0};
    );
    reload_transient(masm);
    onlineasm!(*masm.assembler();
        jmp {scratch};
    );
}

/// `scheme_entry(th, fp, argc) -> NativeReturn`: prologue, then a
/// jump to `enter`.
pub fn emit_scheme_entry(masm: &mut Masm, enter: Label, map: Label) {
    let (tc, sfp, ac, rv) = (regs::TC, regs::SFP, regs::AC, regs::RV);
    let (a0, a1, a2) = arg_regs();

    onlineasm!(*masm.assembler();
        move {tc}, {a0};
        move {sfp}, {a1};
        move {ac}, {a2};
        move {rv}, 0;
    );
    // The bail handler reads the entry argc from here.
    let ac_slot = masm.assembler().mem(tc, sabi::TC_AC as i32);
    onlineasm!(*masm.assembler();
        storeq {ac_slot}, {ac};
    );
    #[cfg(target_arch = "aarch64")]
    {
        let ap = regs::AP;
        let cursor = masm.assembler().mem(tc, sabi::TC_AP as i32);
        onlineasm!(*masm.assembler();
            loadp {ap}, {cursor};
        );
    }

    let (number_tag, not_cell) = (regs::NUMBER_TAG, regs::NOT_CELL_MASK);
    let tag = sabi::NUMBER_TAG;
    let mask = sabi::NOT_CELL_MASK;
    onlineasm!(*masm.assembler();
        move {number_tag}, {tag};
        move {not_cell}, {mask};
    );

    #[cfg(target_arch = "aarch64")]
    {
        let dispatch = regs::DISPATCH;
        onlineasm!(*masm.assembler();
            leap {dispatch}, {map};
        );
    }
    #[cfg(not(target_arch = "aarch64"))]
    {
        let _ = map;
    }
    onlineasm!(*masm.assembler();
        jmp {enter};
    );
}

/// `scheme_exit`: load the `NativeReturn` that `capy_bail` staged in
/// `Tc::{ac, rv}` into the C return registers, then the epilogue.
pub fn emit_exit_glue(masm: &mut Masm) {
    let tc = regs::TC;
    let ac = masm.assembler().mem(tc, sabi::TC_AC as i32);
    let rv = masm.assembler().mem(tc, sabi::TC_RV as i32);
    let (r0, r1) = ret_regs();
    onlineasm!(*masm.assembler();
        loadq {r0}, {ac};
        loadq {r1}, {rv};
    );
    masm.scheme_exit_epilogue();
}

/// `interpreter_enter`: `CP` from `fp[1]`, then the interpreter entry (which
/// heals a closure of compiled code and jumps to it when the JIT is on).
/// Returns the label just after the `CP` reload, for callers that already
/// set `CP` .
pub fn emit_interpreter_enter(masm: &mut Masm, map: Label) -> Label {
    let (sfp, cp) = (regs::SFP, regs::CP);
    onlineasm!(*masm.assembler();
        loadq {cp}, [{sfp} + 8];
    );
    let cp_set = masm.label();
    masm.bind(cp_set);
    super::jit::emit_interpreter_entry(masm, |masm| {
        emit_dispatch(masm, map);
    });
    cp_set
}

/// `interpreter_return`: the bytecode return point is in `fp[-1]`, the `return-point`
/// handler itself pops and reloads `CONSTANT_POINTER`/`MD`.
pub fn emit_interpreter_return(masm: &mut Masm, map: Label) {
    let (sfp, pc) = (regs::SFP, regs::PC);
    onlineasm!(*masm.assembler();
        loadp {pc}, [{sfp} - 8];
    );
    emit_dispatch(masm, map);
}

/// `interpreter_resume`: a control path staged the resume state in `Tc`, reload
/// `PC` from `Tc::ip` and `CONSTANT_POINTER`/`MD` from the closure in `Tc::cp` then dispatch.
pub fn emit_interpreter_resume(masm: &mut Masm, map: Label) {
    let (tc, pc) = (regs::TC, regs::PC);
    let (constant_pointer, md) = (regs::CONSTANT_POINTER, regs::MD);
    let tmp = super::template::tmp::T0;
    let ip = masm.assembler().mem(tc, sabi::TC_IP as i32);
    let cp = masm.assembler().mem(tc, sabi::TC_CP as i32);
    let block_at = masm.assembler().mem(tmp, sabi::CLOSURE_CODE_BLOCK as i32);
    let entry_at = masm
        .assembler()
        .mem(tmp, sabi::CODE_BLOCK_ENTRYPOINT as i32);
    let md_at = masm.assembler().mem(constant_pointer, -8);
    onlineasm!(*masm.assembler();
        loadq {tmp}, {cp};
        loadq {tmp}, {block_at};
        loadp {constant_pointer}, {entry_at};
        loadp {md}, {md_at};
        loadp {pc}, {ip};
    );
    emit_dispatch(masm, map);
}

/// One bail handler for opcode `op`: flush the resume count, save state,
/// store `PC` to `Tc::ip`, and call `bail`.
pub fn emit_bail_handler(masm: &mut Masm, bail: Extern, op: u8, _width: u8) {
    // The resume count `w`: the argument count at an entry, the value count
    // at a return point, ignored everywhere else. Templates keep `AC` holding
    // it at every bail site, except the calls, which pass the caller
    // metadata area instead: at `tail-call` the frame holds the callee in `r1`,
    // so `fp[1]` cannot name the running code (`call` passes it too and
    // asserts it matches).
    let (tc, pc, ac) = (regs::TC, regs::PC, regs::AC);
    if op == def::OpcodeId::OpCall as u8
        || op == def::OpcodeId::OpTailCall as u8
        || op == def::OpcodeId::OpCallSelf as u8
        || op == def::OpcodeId::OpTailCallSelf as u8
    {
        let md = regs::MD;
        onlineasm!(*masm.assembler();
            move {ac}, {md};
        );
    }
    let resume = masm.assembler().mem(tc, sabi::TC_AC as i32);
    onlineasm!(*masm.assembler();
        storeq {resume}, {ac};
    );
    masm.save_scheme_state();
    let ip = masm.assembler().mem(tc, sabi::TC_IP as i32);
    let (a0, a1, a2) = arg_regs();
    onlineasm!(*masm.assembler();
        storep {ip}, {pc};
        move {a0}, {tc};
        move {a1}, {op as i64};
        move {a2}, {_width};
        call {bail};
    );
    masm.restore_scheme_state();
    emit_control_tail(masm);
}

/// One runtime stub: save state, call its control path, and jump to the
/// address it returns.
pub fn emit_stub(masm: &mut Masm, path: Extern) {
    // A stub is entered by a jump that carries `AC` (the argument or value
    // count), `RV` (a single value) and `CP` (the callee).
    let tc = regs::TC;
    let ac = masm.assembler().mem(tc, sabi::TC_AC as i32);
    let rv = masm.assembler().mem(tc, sabi::TC_RV as i32);
    let cp = masm.assembler().mem(tc, sabi::TC_CP as i32);
    let (ac_register, rv_register, cp_register) = (regs::AC, regs::RV, regs::CP);
    onlineasm!(*masm.assembler();
        storeq {ac}, {ac_register};
        storeq {rv}, {rv_register};
        storeq {cp}, {cp_register};
    );
    masm.save_scheme_state();
    let (a0, _, _) = arg_regs();
    let tc = regs::TC;
    onlineasm!(*masm.assembler();
        move {a0}, {tc};
        call {path};
    );
    masm.restore_scheme_state();
    emit_control_tail(masm);
}

/// The machine entry of a native procedure (`subr_call`, installed as
/// `closure.code` for natives). Entered by jump with `SFP` = the call's
/// frame, `CP` = the closure, `AC` = the argument count.
pub(crate) fn emit_subr_call(masm: &mut Masm, path: Extern) {
    let tc = regs::TC;
    let sfp = regs::SFP;
    let (ac_register, rv_register, cp_register) = (regs::AC, regs::RV, regs::CP);
    let (a0, a1, a2) = arg_regs();
    let (r0, _) = ret_regs();
    let return_one = ReturnCode::ReturnOne as i64;
    let enter_body = ReturnCode::EnterBody as i64;
    let ac_at = masm.assembler().mem(tc, sabi::TC_AC as i32);
    let rv_at = masm.assembler().mem(tc, sabi::TC_RV as i32);
    let cp_at = masm.assembler().mem(tc, sabi::TC_CP as i32);
    let sp_at = masm.assembler().mem(tc, sabi::TC_STACK_SP as i32);
    let free_at = masm.assembler().mem(cp_register, sabi::CLOSURE_FREE as i32);
    let proc_at = masm.assembler().mem(a2, sabi::NATIVE_PROC_ADDRESS as i32);
    let ra_at = masm.assembler().mem(sfp, 0);
    let cold = masm.label();
    let body = masm.label();
    let sp_slot = masm.assembler().mem_indexed(sfp, ac_register, 8, 16);
    onlineasm!(*masm.assembler();
        storeq {ac_at}, {ac_register};
        storeq {rv_at}, {rv_register};
        storeq {cp_at}, {cp_register};
        leap {a2}, {sp_slot};
        storep {sp_at}, {a2};
        loadq {a2}, {free_at};
        loadq {a2}, {proc_at};
        move {a1}, {sfp};
        move {a0}, {tc};
    );
    masm.save_scheme_state();
    onlineasm!(*masm.assembler();
        call {a2};
        bqneq {r0}, {return_one}, {body};
    );
    masm.restore_scheme_state();
    onlineasm!(*masm.assembler();
        loadq {rv_register}, [{sfp} + 16];
        move {ac_register}, 1;
        jmp {ra_at};
    );
    masm.bind(body);
    let jit_entry = masm
        .assembler()
        .mem(tmp::T3, sabi::CODE_BLOCK_JIT_ENTRY as i32);
    let block_at = masm
        .assembler()
        .mem(cp_register, sabi::CLOSURE_CODE_BLOCK as i32);
    let code_at = masm.assembler().mem(cp_register, sabi::CLOSURE_CODE as i32);
    let generic_entry = masm.label();
    onlineasm!(*masm.assembler();
        bqneq {r0}, {enter_body}, {cold};
    );
    masm.restore_scheme_state();
    onlineasm!(*masm.assembler();
        loadq {cp_register}, [{sfp} + 8];
        loadq {ac_register}, {ac_at};
        loadp {tmp::T3}, {block_at};
        loadp {tmp::T2}, {jit_entry};
        btpz {tmp::T2}, {generic_entry};
        bpeq {tmp::T2}, -1, {generic_entry};
        jmp {tmp::T2};
    );
    masm.bind(generic_entry);
    onlineasm!(*masm.assembler();
        jmp {code_at};
    );
    masm.bind(cold);
    onlineasm!(*masm.assembler();
        move {a1}, {r0};
        move {a0}, {tc};
        call {path};
    );
    masm.restore_scheme_state();
    emit_control_tail(masm);
}

/// The machine entry of a generic function closure. Entered like
/// `subr_call` (`SFP` = the call's frame, `CP` = the closure, `AC` = the
/// argument count).
pub(crate) fn emit_generic_call(masm: &mut Masm, subr_call: Label) {
    use crate::runtime::class::generic::slot_cache_layout as layout;
    let (sfp, cp, ac, rv, tc) = (regs::SFP, regs::CP, regs::AC, regs::RV, regs::TC);
    let (t0, t1, t2, t3) = (tmp::T0, tmp::T1, tmp::T2, tmp::T3);
    let class_mask = sabi::CLASS_ID_MASK as i64;
    let (top, arity_ok, looping, next, reset) = (
        masm.label(),
        masm.label(),
        masm.label(),
        masm.label(),
        masm.label(),
    );
    let (writer_path, barrier) = (masm.label(), masm.label());
    let log = masm.declare_rust_fn(RuntimeExtern::LogObject);

    let arg_at = masm.assembler().mem(sfp, 16);
    let value_at = masm.assembler().mem(sfp, 24);
    let header_at = masm.assembler().mem(t1, sabi::HEADER_OFFSET as i32);
    let generic_at = masm.assembler().mem(cp, sabi::CLOSURE_FREE as i32 + 8);
    let cache_at = masm.assembler().mem(t0, layout::FIELD as i32);
    let len_at = masm.assembler().mem(t0, 0);
    masm.bind(top);
    onlineasm!(*masm.assembler();
        bqeq {ac}, 1, {arity_ok};
        bqneq {ac}, 2, {subr_call};
    );
    masm.bind(arity_ok);
    onlineasm!(*masm.assembler();
        loadq {t1}, {arg_at};
    );
    masm.branch_if_not_cell(t1, t0, subr_call);
    onlineasm!(*masm.assembler();
        loadq {t2}, {header_at};
        andq {t2}, {t2}, {class_mask};
        loadq {t0}, {generic_at};
        loadq {t0}, {cache_at};
        loadq {t3}, {len_at};
        bqeq {t3}, 0, {reset};
        addp {t0}, {t0}, {layout::ARRAY_DATA as i64};
    );
    masm.bind(looping);
    let class_at = masm.assembler().mem(t0, layout::CLASS_ID as i32);
    let writer_at = masm.assembler().mem(t0, layout::WRITER as i32);
    let stamp_at = masm.assembler().mem(t0, layout::STAMP as i32);
    let index_at = masm.assembler().mem(t0, layout::INDEX as i32);
    let storage_at = masm.assembler().mem(t1, 8);
    let storage_stamp_at = masm.assembler().mem(t1, 0);

    onlineasm!(*masm.assembler();
        loadi {rv}, {class_at};
        bqneq {rv}, {t2}, {next};
        loadb {t3}, {writer_at};
        loadi {rv}, {stamp_at};
        loadq {t1}, {storage_at};
        loadi {t2}, {storage_stamp_at};
        bqneq {t2}, {rv}, {reset};
        bqneq {t3}, 0, {writer_path};
        bqneq {ac}, 1, {reset};
        loadi {t3}, {index_at};
    );
    let slot = masm
        .assembler()
        .mem_indexed(t1, t3, 8, layout::ARRAY_DATA as i32);
    let ra_at = masm.assembler().mem(sfp, 0);
    onlineasm!(*masm.assembler();
        loadq {rv}, {slot};
        bqeq {rv}, 0, {reset};
        storeq {arg_at}, {rv};
        move {ac}, 1;
        jmp {ra_at};
    );
    // A writer entry: the store goes to the slot storage, which the unlog
    // bit guards.
    masm.bind(writer_path);
    onlineasm!(*masm.assembler();
        bqneq {ac}, 2, {reset};
        loadi {t3}, {index_at};
    );
    GcCodegen::new().emit_unlogged_check(masm, t1, rv, t2, barrier);
    onlineasm!(*masm.assembler();
        loadq {rv}, {value_at};
        storeq {slot}, {rv};
        storeq {arg_at}, {rv};
        move {ac}, 1;
        jmp {ra_at};
    );
    masm.bind(barrier);
    let (a0, a1, _) = arg_regs();
    let ac_slot = masm.assembler().mem(tc, sabi::TC_AC as i32);
    let closure_at = masm.assembler().mem(sfp, 8);
    onlineasm!(*masm.assembler();
        storeq {ac_slot}, {ac};
        move {a1}, {t1};
        move {a0}, {tc};
    );
    masm.call_rust(log);
    onlineasm!(*masm.assembler();
        loadq {ac}, {ac_slot};
        loadq {cp}, {closure_at};
        jmp {top};
    );
    masm.bind(next);
    onlineasm!(*masm.assembler();
        addp {t0}, {t0}, {layout::ENTRY_SIZE as i64};
        subq {t3}, {t3}, 1;
        bqneq {t3}, 0, {looping};
    );
    // No slot entry served the call: try the method cache
    masm.bind(reset);
    {
        use crate::runtime::class::generic::method_cache_layout as ml;
        let (mloop, mnext, mhit, one, generic_entry, fail, giveup, entered) = (
            masm.label(),
            masm.label(),
            masm.label(),
            masm.label(),
            masm.label(),
            masm.label(),
            masm.label(),
            masm.label(),
        );
        let arg1_at = masm.assembler().mem(sfp, 24);
        let arg2_at = masm.assembler().mem(sfp, 32);
        let closure_slot = masm.assembler().mem(sfp, 8);
        let lead_slot = masm.assembler().mem(sfp, 16);
        let header1_at = masm.assembler().mem(t1, sabi::HEADER_OFFSET as i32);
        let mcache_at = masm.assembler().mem(t0, ml::FIELD as i32);
        let mlen_at = masm.assembler().mem(t0, 0);
        let argc_at = masm.assembler().mem(t0, ml::ARGC as i32);
        let class0_at = masm.assembler().mem(t0, ml::CLASS0 as i32);
        let class1_at = masm.assembler().mem(t0, ml::CLASS1 as i32);
        let body_at = masm.assembler().mem(t0, ml::BODY as i32);
        let block_at = masm.assembler().mem(cp, sabi::CLOSURE_CODE_BLOCK as i32);
        let code_at = masm.assembler().mem(cp, sabi::CLOSURE_CODE as i32);
        let jit_entry = masm.assembler().mem(t3, sabi::CODE_BLOCK_JIT_ENTRY as i32);
        let false_bits = Value::new(false).bits() as i64;
        onlineasm!(*masm.assembler();
            bqeq {ac}, 1, {mhit};
            bqneq {ac}, 2, {giveup};
        );
        masm.bind(mhit);
        onlineasm!(*masm.assembler();
            loadq {t1}, {arg_at};
        );
        masm.branch_if_not_cell(t1, t0, giveup);
        onlineasm!(*masm.assembler();
            loadq {t2}, {header1_at};
            andq {t2}, {t2}, {class_mask};
            loadq {t0}, {generic_at};
            loadq {t0}, {mcache_at};
            loadq {t3}, {mlen_at};
            bqeq {t3}, 0, {fail};
            addp {t0}, {t0}, {ml::ARRAY_DATA as i64};
        );
        masm.bind(mloop);
        onlineasm!(*masm.assembler();
            loadi {rv}, {argc_at};
            bqneq {rv}, {ac}, {mnext};
            loadi {rv}, {class0_at};
            bqneq {rv}, {t2}, {mnext};
            bqeq {ac}, 1, {one};
            loadq {t1}, {arg1_at};
        );
        masm.branch_if_not_cell(t1, rv, mnext);
        onlineasm!(*masm.assembler();
            loadq {t1}, {header1_at};
            andq {t1}, {t1}, {class_mask};
            loadi {rv}, {class1_at};
            bqneq {rv}, {t1}, {mnext};
        );
        masm.bind(one);
        // Room for the shifted frame, as a callee's prologue checks it.
        onlineasm!(*masm.assembler();
            move {t1}, {ac};
            addq {t1}, {t1}, 3;
        );
        masm.check_frame_limit(t1, t2, t3, giveup);
        onlineasm!(*masm.assembler();
            loadq {t1}, {arg_at};
            bqeq {ac}, 1, {generic_entry};
            loadq {t2}, {arg1_at};
            storeq {arg2_at}, {t2};
        );
        masm.bind(generic_entry);
        onlineasm!(*masm.assembler();
            storeq {arg1_at}, {t1};
            move {t1}, {false_bits};
            storeq {lead_slot}, {t1};
            loadq {cp}, {body_at};
        );
        // Both caches enter here with the method body in `cp`.
        masm.bind(entered);
        onlineasm!(*masm.assembler();
            storeq {closure_slot}, {cp};
            addq {ac}, {ac}, 1;
            loadp {t3}, {block_at};
            loadp {t2}, {jit_entry};
        );
        let enter_code = masm.label();
        onlineasm!(*masm.assembler();
            btpz {t2}, {enter_code};
            bpeq {t2}, -1, {enter_code};
            jmp {t2};
        );
        masm.bind(enter_code);
        onlineasm!(*masm.assembler();
            jmp {code_at};
        );
        masm.bind(mnext);
        onlineasm!(*masm.assembler();
            addp {t0}, {t0}, {ml::ENTRY_SIZE as i64};
            subq {t3}, {t3}, 1;
            bqneq {t3}, 0, {mloop};
        );
        // The method cache missed: probe the open-addressed dispatch table.
        // Generics dispatching on more than two arguments have `rest` ids
        // the probe does not compare, and stay on the Rust path.
        masm.bind(fail);
        {
            use crate::runtime::class::generic::{DISPATCH_MUL, dispatch_table_layout as dl};
            let (tstart, capped, nokey1, probe, advance, thit, tentry) = (
                masm.label(),
                masm.label(),
                masm.label(),
                masm.label(),
                masm.label(),
                masm.label(),
                masm.label(),
            );
            let width_at = masm.assembler().mem(t0, dl::WIDTH as i32);
            let cap_at = masm.assembler().mem(t0, dl::ARITY_CAP as i32);
            let table_at = masm.assembler().mem(t0, dl::FIELD as i32);
            let tlen_at = masm.assembler().mem(t0, 0);
            let methods_at = masm
                .assembler()
                .mem(rv, (dl::ARRAY_DATA + dl::METHODS) as i32);
            let key_at = masm.assembler().mem(rv, (dl::ARRAY_DATA + dl::KEY) as i32);
            let flags_at = masm
                .assembler()
                .mem(rv, (dl::ARRAY_DATA + dl::FLAGS) as i32);
            let tbody_at = masm.assembler().mem(rv, (dl::ARRAY_DATA + dl::BODY) as i32);
            let mul0 = DISPATCH_MUL[0] as i64;
            let bucket_size = dl::BUCKET_SIZE as i64;
            let class_mask = sabi::CLASS_ID_MASK as i64;
            onlineasm!(*masm.assembler();
                bqeq {ac}, 1, {tstart};
                bqneq {ac}, 2, {giveup};
            );
            masm.bind(tstart);
            onlineasm!(*masm.assembler();
                loadq {t0}, {generic_at};
                loadi {t3}, {width_at};
                bqgt {t3}, 2, {giveup};
                loadi {t2}, {cap_at};
                bqlteq {t2}, {ac}, {capped};
                move {t2}, {ac};
            );
            masm.bind(capped);
            onlineasm!(*masm.assembler();
                lshiftq {t2}, 48;
                loadq {t1}, {arg_at};
            );
            masm.branch_if_not_cell(t1, rv, giveup);
            onlineasm!(*masm.assembler();
                loadq {rv}, {header1_at};
                andq {rv}, {rv}, {class_mask};
                orq {t2}, {t2}, {rv};
                bqeq {ac}, 1, {nokey1};
                bqeq {t3}, 1, {nokey1};
                loadq {t1}, {arg1_at};
            );
            masm.branch_if_not_cell(t1, rv, giveup);
            onlineasm!(*masm.assembler();
                loadq {rv}, {header1_at};
                andq {rv}, {rv}, {class_mask};
                lshiftq {rv}, 24;
                orq {t2}, {t2}, {rv};
            );
            masm.bind(nokey1);
            onlineasm!(*masm.assembler();
                loadq {t0}, {generic_at};
                loadq {t0}, {table_at};
                loadq {t3}, {tlen_at};
                bqeq {t3}, 0, {giveup};
                subq {t3}, {t3}, 1;
                move {t1}, {mul0};
                mulq {t1}, {t1}, {t2};
                urshiftq {t1}, 32;
                andq {t1}, {t1}, {t3};
            );
            // `t0` table, `t1` index, `t2` key, `rv` the bucket under test.
            masm.bind(probe);
            onlineasm!(*masm.assembler();
                move {rv}, {t1};
                mulq {rv}, {rv}, {bucket_size};
                addp {rv}, {rv}, {t0};
                loadq {t3}, {methods_at};
                bqeq {t3}, 0, {giveup};
                loadq {t3}, {key_at};
                bqneq {t3}, {t2}, {advance};
                loadi {t3}, {flags_at};
                btqz {t3}, {dl::DIRECT}, {giveup};
                loadq {rv}, {tbody_at};
                jmp {thit};
            );
            masm.bind(advance);
            onlineasm!(*masm.assembler();
                loadq {t3}, {tlen_at};
                subq {t3}, {t3}, 1;
                addq {t1}, {t1}, 1;
                andq {t1}, {t1}, {t3};
                jmp {probe};
            );
            // A `NO_NEXT` closure body in `rv`: enter it as the method
            // cache does.
            masm.bind(thit);
            onlineasm!(*masm.assembler();
                move {t1}, {ac};
                addq {t1}, {t1}, 3;
            );
            masm.check_frame_limit(t1, t2, t3, giveup);
            onlineasm!(*masm.assembler();
                loadq {t1}, {arg_at};
                bqeq {ac}, 1, {tentry};
                loadq {t2}, {arg1_at};
                storeq {arg2_at}, {t2};
            );
            masm.bind(tentry);
            onlineasm!(*masm.assembler();
                storeq {arg1_at}, {t1};
                move {t1}, {false_bits};
                storeq {lead_slot}, {t1};
                move {cp}, {rv};
                jmp {entered};
            );
        }
        masm.bind(giveup);
        onlineasm!(*masm.assembler();
            move {rv}, 0;
            jmp {subr_call};
        );
        let _ = (mnext, generic_entry);
    }
}

//
// A compiled `call`/`tail-call` keeps its monomorphic fast path at the site: the
// callee's code block against `CallLinkInfo::callee`. The second distinct
// callee lazily generates a polymorphic stub for the site, referenced from
// `CallLinkInfo::stub`: a small image comparing the callee's code block
// against the site's case slots and entering the match. One more distinct
// callee than the stub holds turns the site megamorphic: the template tests
// `polymorphic == LINK_MEGA` before the stub jump, so the site stays on the
// generic inline path and never reaches the stub again.
//
// A hit enters the block's compiled code directly when it has some, else
// goes through `closure.code` (the interpreter entry, which heals, or
// `subr_call` for a native).

/// One generated call IC stub: the image its site's `stub` word points
/// into. Owned by the stub's [`CodeImage`]
pub(crate) struct CallIcStub {
    pub(crate) image: LoadedImage,
}

/// Serializes the writers of call sites: stub generation and case fills.
/// Readers (the stubs themselves) take no lock.
static SITE_WRITES: Mutex<()> = Mutex::new(());

/// The bases of the stubs alive, for tests.
#[cfg(test)]
static LIVE_STUBS: std::sync::LazyLock<Mutex<std::collections::HashSet<Address>>> =
    std::sync::LazyLock::new(Default::default);

#[cfg(test)]
impl Drop for CallIcStub {
    fn drop(&mut self) {
        LIVE_STUBS.lock().remove(&self.image.base());
    }
}

/// Whether the stub at `base` is still loaded, for tests.
#[cfg(test)]
pub fn stub_alive(base: Address) -> bool {
    super::reclaim::drain();
    LIVE_STUBS.lock().contains(&base)
}

/// Call IC misses taken, for tests asserting a site stays off the miss path.
#[cfg(test)]
static IC_MISSES: AtomicU64 = AtomicU64::new(0);

/// The `call` miss path: a link miss in compiled code. The template staged
/// `argc` in `Tc::ac`, `frame` is the callee frame and `link` the site.
/// Enters through the shared control core like `capy_call_slow`, and grows
/// the site mono -> poly -> megamorphic.
pub(crate) unsafe extern "C" fn capy_ic_miss_call(
    th: *const (),
    frame: Slot<'static>,
    link: *const CallLinkInfo,
) -> *const u8 {
    unsafe { ic_miss(th, frame, link, true) }
}

/// The `tail-call` miss path: like [`capy_ic_miss_call`], with the frame in
/// `SFP` instead of a dedicated register.
pub(crate) unsafe extern "C" fn capy_ic_miss_tcall(
    th: *const (),
    frame: Slot<'static>,
    link: *const CallLinkInfo,
) -> *const u8 {
    unsafe { ic_miss(th, frame, link, false) }
}

unsafe fn ic_miss(
    th: *const (),
    frame: Slot<'static>,
    link: *const CallLinkInfo,
    is_call: bool,
) -> *const u8 {
    #[cfg(test)]
    IC_MISSES.fetch_add(1, Relaxed);
    unsafe {
        let mutator = MutationContext::from_ptr(th);
        let tc: &Tc<'static> = mutator.tc();
        let argc = tc.ac.get() as usize;
        let link = &*link;
        let callee = *frame.add(1);
        let buffer = link.buffer().map(Value::from_gc);
        // A second (or later) distinct closure callee grows the stub. The
        // mono record itself stays with `record_link` inside `control_enter`,
        // which the site below carries the buffer for, non-closures keep the
        // generic-dispatch cache behavior of `capy_call_slow` untouched.
        if let (Some(buffer), true) = (buffer, callee.is::<Closure>()) {
            let block = Value::from_gc(Closure::code_block(Closure::from_value(callee)));
            let current = link.callee.bits();
            if current != 0 && current != block.bits() && !link.is_megamorphic() {
                ensure_poly(mutator, link, buffer, block, is_call);
            }
        }
        let site = buffer.map(|buffer| slow::EnterLink { link, buffer });
        debug_assert!(site.is_some(), "JIT call site without its buffer");
        stage_ic_control(th, slow::control_enter(mutator, frame, callee, argc, site))
    }
}

/// A distinct callee arrived at a linked site: generate the site's stub
/// when the second one arrives, then record its code block as a case,
/// marking the site megamorphic when the cases are full. Idempotent: races
/// only re-record what is already there.
fn ensure_poly<'gc>(
    mutator: MutationContext<'gc>,
    link: &CallLinkInfo,
    buffer: Value<'gc>,
    block: Value<'gc>,
    is_call: bool,
) {
    super::reclaim::drain();
    if link.stub.load(Acquire).is_zero() {
        match generate_stub(mutator, link, is_call) {
            Ok(built) => publish_or_free(link, buffer, built, mutator),
            Err(err) => {
                log::warn!(target: crate::heap::logging::JIT_LOG_TARGET, "call IC stub: {err}");
                return;
            }
        }
    }
    let _writes = SITE_WRITES.lock();
    if link.stub.load(Relaxed).is_zero() {
        return;
    }
    let barrier = Gc::write(mutator, buffer.downcast::<CodeBuffer>());
    if insert_case(barrier, link, block) == CaseInsert::Full {
        link.polymorphic.store(LINK_MEGA, Relaxed);
    }
}

/// How [`insert_case`] resolved a code block.
#[derive(PartialEq, Eq, Debug)]
enum CaseInsert {
    /// Already a case.
    Present,
    /// Stored into an empty slot.
    Added,
    /// No empty slot left: the site is megamorphic.
    Full,
}

/// Record `block` in the site's cases, or report them full. The first
/// empty slot is seeded with `link.callee` first, so the monomorphic
/// callee stays served once the stub takes over. The caller holds
/// `SITE_WRITES`, so writers serialize. The stub's plain loads may lag a fill, which only misses
/// to the slow path and re-records.
fn insert_case(
    barrier: &Write<CodeBuffer<'_>>,
    link: &CallLinkInfo,
    block: Value<'_>,
) -> CaseInsert {
    let mut result = CaseInsert::Full;
    for wanted in [link.callee.get_value(), Some(block)].into_iter().flatten() {
        result = insert_one(barrier, &link.cases, wanted);
    }
    result
}

fn insert_one(
    barrier: &Write<CodeBuffer<'_>>,
    cases: &[GcCell<CodeBlock<'static>>; CALL_IC_CASES],
    block: Value<'_>,
) -> CaseInsert {
    let mut empty = None;
    for (index, case) in cases.iter().enumerate() {
        let found = case.bits();
        if found == block.bits() {
            return CaseInsert::Present;
        }
        if found == 0 && empty.is_none() {
            empty = Some(index);
        }
    }
    match empty {
        Some(index) => {
            cases[index].set_value(barrier, Some(block));
            CaseInsert::Added
        }
        None => CaseInsert::Full,
    }
}

/// A generated but unpublished stub: the owner that holds it.
struct BuiltStub<'gc> {
    base: Address,
    owner: Gc<'gc, CodeImage<'gc>>,
}

/// Generate the `call`/`tail-call` stub for the site at `link`.
fn generate_stub<'gc>(
    mutator: MutationContext<'gc>,
    link: &CallLinkInfo,
    is_call: bool,
) -> io::Result<BuiltStub<'gc>> {
    let mut masm = Masm::new();
    let start = masm.label();
    masm.bind(start);
    let miss = masm.declare_rust_fn(if is_call {
        RuntimeExtern::IcMissCall
    } else {
        RuntimeExtern::IcMissTcall
    });
    emit_call_ic_stub(&mut masm, link, miss, is_call);
    let name = if is_call {
        "call_ic_stub"
    } else {
        "tcall_ic_stub"
    };
    let loaded = StubBuilder::new(masm, name, start, SymbolKind::Stub)
        .with_cfi(sabi::SCHEME_ENTRY_CFA_OFFSET as u32)
        .finish()?
        .load()?;
    let base = loaded.base();
    #[cfg(test)]
    LIVE_STUBS.lock().insert(base);
    // The owner: a non-moving object that holds the stub, finalized to
    // release it.
    let owner = CodeImage::alloc(mutator, Native::Stub(CallIcStub { image: loaded }));
    Ok(BuiltStub { base, owner })
}

/// Publish a generated stub at its site, or drop it when a racing thread
/// published first.
fn publish_or_free<'gc>(
    link: &CallLinkInfo,
    buffer: Value<'gc>,
    built: BuiltStub<'gc>,
    mutator: MutationContext<'gc>,
) {
    let writes = SITE_WRITES.lock();
    if !link.stub.load(Relaxed).is_zero() {
        // The loser's owner is unreferenced: the collector finds it dead.
        return;
    }
    let base = built.base;
    drop(writes);
    let barrier = Gc::write(mutator, buffer.downcast::<CodeBuffer>());
    link.stub_owner.set(barrier, Some(built.owner));
    link.stub.store(base, Release);
}

/// Emit one call IC stub for the site at `link`. Entered by a jump carrying
/// the callee's code block in `T3`, the closure in `CP` and `argc` in `AC`,
/// plus the callee frame in `T1` for `call` (`SFP` already points at it for
/// `tail-call`). Compares the block against each traced case slot of the site,
/// a hit enters the block's compiled code directly when it has some, else
/// goes through `closure.code`. Anything else misses to `miss`.
pub fn emit_call_ic_stub(masm: &mut Masm, link: &CallLinkInfo, miss: Extern, is_call: bool) {
    let link = imm(Address::from_ref(link));
    let (cp, ac, sfp) = (regs::CP, regs::AC, regs::SFP);
    let (t0, t1, t2, t3) = (tmp::T0, tmp::T1, tmp::T2, tmp::T3);
    let hit = masm.label();
    let cases = core::mem::offset_of!(CallLinkInfo, cases);
    onlineasm!(*masm.assembler();
        move {t0}, {link};
    );
    for index in 0..CALL_IC_CASES {
        let slot = masm.assembler().mem(t0, (cases + index * 8) as i32);
        onlineasm!(*masm.assembler();
            loadq {t2}, {slot};
            bqeq {t2}, {t3}, {hit};
        );
    }
    // A miss: `argc` to its `Tc` slot, then the miss path as
    // `(th, frame, link)`. The link moves first because it is baked, and
    // the frame before `save_scheme_state`, which repoints `SFP` on x86-64.
    let ac_slot = masm.assembler().mem(regs::TC, sabi::TC_AC as i32);
    let (a0, a1, a2) = arg_regs();
    let frame = if is_call { t1 } else { sfp };
    onlineasm!(*masm.assembler();
        storeq {ac_slot}, {ac};
        move {a2}, {link};
        move {a1}, {frame};
        move {a0}, {regs::TC};
    );
    masm.save_scheme_state();
    onlineasm!(*masm.assembler();
        call {miss};
    );
    masm.restore_scheme_state();
    emit_control_tail(masm);
    // A hit: point `SFP` at the callee frame (a `tail-call` already does), then
    // enter the block's compiled code when it has some (`jit_entry` is 0
    // before compiling and all ones when it never will), otherwise the
    // closure's machine entry, like `enter_callee`.
    masm.bind(hit);
    if is_call {
        onlineasm!(*masm.assembler();
            move {sfp}, {t1};
        );
    }
    let generic = masm.label();
    let jit_entry = masm.assembler().mem(t3, sabi::CODE_BLOCK_JIT_ENTRY as i32);
    let code_at = masm.assembler().mem(cp, sabi::CLOSURE_CODE as i32);
    onlineasm!(*masm.assembler();
        loadp {t2}, {jit_entry};
        btpz {t2}, {generic};
        bpeq {t2}, -1, {generic};
        jmp {t2};
    );
    masm.bind(generic);
    onlineasm!(*masm.assembler();
        jmp {code_at};
    );
}

unsafe fn stage_ic_control(th: *const (), control: slow::Control<'static>) -> *const u8 {
    // SAFETY: forwarded from the caller.
    unsafe { super::slow::stage_control_with(th, control, Some(Value::undefined())) }
}

/// Which sparse switch table [`emit_switch_search`] searches: `(key:i32,
/// disp:i32)` pairs for `switch-fixnum`/`switch_char`, `(hash_lo, hash_hi,
/// disp)` triples for `switch-symbol`.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SwitchSearchKind {
    Int,
    Sym,
}

/// A sparse switch-table search, emitted at the current position and
/// entered with `call`
///
/// Convention: `T0` is the entries pointer, `T1` the count, `T2` the key
/// (the low 32 bits for [`SwitchSearchKind::Int`], the masked hash for
/// [`SwitchSearchKind::Sym`]). Returns with `T1` set to 1 and `T2` to the
/// sign-extended displacement on a hit, `T1` set to 0 on a miss (the
/// caller loads `default` itself, off the hot path). Clobbers `T0`..`T3`,
/// every SABI register is preserved.
pub fn emit_switch_search(masm: &mut Masm, kind: SwitchSearchKind) {
    let (search, hit, miss, greater) = (masm.label(), masm.label(), masm.label(), masm.label());
    masm.bind(search);
    match kind {
        SwitchSearchKind::Int => {
            let disp = masm.assembler().mem_indexed(tmp::T0, tmp::T3, 8, 4);
            onlineasm!(*masm.assembler();
                btqz {tmp::T1}, {miss};
                move {tmp::T3}, {tmp::T1};
                urshiftq {tmp::T3}, 1;
                lshiftq {tmp::T3}, 3;
                addp {tmp::T3}, {tmp::T3}, {tmp::T0};
                loadi {tmp::T3}, [{tmp::T3}];
                bieq {tmp::T3}, {tmp::T2}, {hit};
                bilt {tmp::T3}, {tmp::T2}, {greater};
                urshiftq {tmp::T1}, 1;
                jmp {search};
            );
            masm.bind(greater);
            onlineasm!(*masm.assembler();
                move {tmp::T3}, {tmp::T1};
                urshiftq {tmp::T3}, 1;
                subq {tmp::T1}, {tmp::T1}, {tmp::T3};
                subq {tmp::T1}, {tmp::T1}, 1;
                lshiftq {tmp::T3}, 3;
                addq {tmp::T3}, {tmp::T3}, 8;
                addp {tmp::T0}, {tmp::T0}, {tmp::T3};
                jmp {search};
            );
            masm.bind(hit);
            onlineasm!(*masm.assembler();
                move {tmp::T3}, {tmp::T1};
                urshiftq {tmp::T3}, 1;
                loadis {tmp::T2}, {disp};
                move {tmp::T1}, 1;
                ret;
            );
        }
        SwitchSearchKind::Sym => {
            onlineasm!(*masm.assembler();
                btqz {tmp::T1}, {miss};
                move {tmp::T3}, {tmp::T1};
                urshiftq {tmp::T3}, 1;
                mulq {tmp::T3}, {tmp::T3}, 12;
                addp {tmp::T3}, {tmp::T3}, {tmp::T0};
                loadq {tmp::T3}, [{tmp::T3}];
                bqeq {tmp::T3}, {tmp::T2}, {hit};
                bqb {tmp::T3}, {tmp::T2}, {greater};
                urshiftq {tmp::T1}, 1;
                jmp {search};
            );
            masm.bind(greater);
            onlineasm!(*masm.assembler();
                move {tmp::T3}, {tmp::T1};
                urshiftq {tmp::T3}, 1;
                subq {tmp::T1}, {tmp::T1}, {tmp::T3};
                subq {tmp::T1}, {tmp::T1}, 1;
                mulq {tmp::T3}, {tmp::T3}, 12;
                addq {tmp::T3}, {tmp::T3}, 12;
                addp {tmp::T0}, {tmp::T0}, {tmp::T3};
                jmp {search};
            );
            masm.bind(hit);
            onlineasm!(*masm.assembler();
                move {tmp::T3}, {tmp::T1};
                urshiftq {tmp::T3}, 1;
                mulq {tmp::T3}, {tmp::T3}, 12;
                addp {tmp::T3}, {tmp::T3}, {tmp::T0};
                loadis {tmp::T2}, [{tmp::T3} + 8];
                move {tmp::T1}, 1;
                ret;
            );
        }
    }
    masm.bind(miss);
    onlineasm!(*masm.assembler();
        move {tmp::T1}, 0;
        ret;
    );
}
