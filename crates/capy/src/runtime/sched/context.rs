//! Stackful context switch for threads.

use mmtk::util::Address;

use super::scheduler::virtual_thread_entry;
use crate::heap::sync::thread::VirtualThread;

/// The stack pointer of a suspended context.
#[repr(C)]
#[derive(Clone, Copy)]
pub(crate) struct SavedContext {
    sp: Address,
}

impl Default for SavedContext {
    fn default() -> Self {
        Self { sp: Address::ZERO }
    }
}

#[cfg(target_vendor = "apple")]
macro_rules! asm_sym {
    ($name:literal) => {
        concat!("_", $name)
    };
}

#[cfg(not(target_vendor = "apple"))]
macro_rules! asm_sym {
    ($name:literal) => {
        $name
    };
}

#[cfg(target_arch = "x86_64")]
core::arch::global_asm!(
    concat!(".globl ", asm_sym!("capy_sched_context_switch")),
    concat!(asm_sym!("capy_sched_context_switch"), ":"),
    // rdi = old, rsi = new. Frame: mxcsr/fpucw, r15, r14, r13, r12, rbx, rbp, ret.
    "push rbp",
    "push rbx",
    "push r12",
    "push r13",
    "push r14",
    "push r15",
    "sub rsp, 8",
    "stmxcsr [rsp]",
    "fnstcw [rsp + 4]",
    "mov [rdi], rsp",
    "mov rsp, [rsi]",
    "ldmxcsr [rsp]",
    "fldcw [rsp + 4]",
    "add rsp, 8",
    "pop r15",
    "pop r14",
    "pop r13",
    "pop r12",
    "pop rbx",
    "pop rbp",
    "ret",
    concat!(".globl ", asm_sym!("capy_sched_entry")),
    concat!(asm_sym!("capy_sched_entry"), ":"),
    // First resume lands here with the thread pointer in r12.
    "mov rdi, r12",
    "and rsp, -16",
    // Poisoned return address: if the entry ever returns, it jumps to 0.
    "push 0",
    "jmp {entry}",
    entry = sym virtual_thread_entry,
);

#[cfg(target_arch = "aarch64")]
core::arch::global_asm!(
    concat!(".globl ", asm_sym!("capy_sched_context_switch")),
    concat!(asm_sym!("capy_sched_context_switch"), ":"),
    // x0 = old, x1 = new. Frame: x19-x28, x29/x30, d8-d15, fpcr; 176 bytes.
    "sub sp, sp, #176",
    "stp x19, x20, [sp, #0]",
    "stp x21, x22, [sp, #16]",
    "stp x23, x24, [sp, #32]",
    "stp x25, x26, [sp, #48]",
    "stp x27, x28, [sp, #64]",
    "stp x29, x30, [sp, #80]",
    "stp d8, d9, [sp, #96]",
    "stp d10, d11, [sp, #112]",
    "stp d12, d13, [sp, #128]",
    "stp d14, d15, [sp, #144]",
    "mrs x2, fpcr",
    "str x2, [sp, #160]",
    "mov x2, sp",
    "str x2, [x0]",
    "ldr x2, [x1]",
    "mov sp, x2",
    "ldr x2, [sp, #160]",
    "msr fpcr, x2",
    "ldp x19, x20, [sp, #0]",
    "ldp x21, x22, [sp, #16]",
    "ldp x23, x24, [sp, #32]",
    "ldp x25, x26, [sp, #48]",
    "ldp x27, x28, [sp, #64]",
    "ldp x29, x30, [sp, #80]",
    "ldp d8, d9, [sp, #96]",
    "ldp d10, d11, [sp, #112]",
    "ldp d12, d13, [sp, #128]",
    "ldp d14, d15, [sp, #144]",
    "add sp, sp, #176",
    "ret",
    concat!(".globl ", asm_sym!("capy_sched_entry")),
    concat!(asm_sym!("capy_sched_entry"), ":"),
    // First resume lands here with the thread pointer in x19.
    "mov x0, x19",
    // Poisoned return address: a return from the entry jumps to 0.
    "mov x30, xzr",
    "b {entry}",
    entry = sym virtual_thread_entry,
);

unsafe extern "C" {
    #[link_name = "capy_sched_context_switch"]
    fn raw_context_switch(old: *mut SavedContext, new: *const SavedContext);
    #[link_name = "capy_sched_entry"]
    fn raw_entry();
}

/// Save the callee-saved state of the running context into `old` and resume
/// `new`. Returns when some context switches back to `old`.
///
/// # Safety
///
/// `new` must hold a context made by [`context_init`] or saved by an earlier
/// call here, on a stack that is still mapped. `old` must stay valid until
/// the context is resumed.
#[inline(never)]
pub(crate) unsafe fn context_switch(old: *mut SavedContext, new: *const SavedContext) {
    unsafe { raw_context_switch(old, new) }
}

/// Prepare `ctx` so that the first switch to it calls the thread entry with
/// `g`. `stack_top` is the high end of a mapped, unused stack.
///
/// # Safety
///
/// `stack_top` must be the end of at least 64 writable bytes that nothing
/// else uses, and `g` must stay valid until the thread finishes.
pub(crate) unsafe fn context_init(
    ctx: *mut SavedContext,
    stack_top: *mut u8,
    g: *mut VirtualThread,
) {
    let top = (stack_top as usize) & !15;
    let entry = raw_entry as *const () as usize;
    unsafe {
        #[cfg(target_arch = "x86_64")]
        {
            // Frame from low to high: mxcsr/fpucw, r15, r14, r13, r12, rbx, rbp, ret.
            let sp = top - 64;
            let words = sp as *mut usize;
            (sp as *mut u32).write(0x1F80);
            ((sp + 4) as *mut u16).write(0x037F);
            words.add(1).write(0);
            words.add(2).write(0);
            words.add(3).write(0);
            words.add(4).write(g as usize);
            words.add(5).write(0);
            words.add(6).write(0);
            words.add(7).write(entry);
            (*ctx).sp = Address::from_usize(sp);
        }
        #[cfg(target_arch = "aarch64")]
        {
            // Frame: x19 (thread), x20-x28, x29, x30 (entry), d8-d15, fpcr.
            let sp = top - 176;
            let words = sp as *mut usize;
            for i in 0..22 {
                words.add(i).write(0);
            }
            words.write(g as usize);
            words.add(11).write(entry);
            (*ctx).sp = Address::from_usize(sp);
        }
    }
}
