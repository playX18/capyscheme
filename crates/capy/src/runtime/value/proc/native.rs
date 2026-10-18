//! Native procedure ABI values.

use super::*;
use crate::runtime::interp::stack::Slot;

pub enum Return<'gc> {
    Continue(Value<'gc>),
    Err(Value<'gc>),
}

pub type NativeFn<'gc> = extern "C" fn(ctx: MutationContext<'gc>, sfp: Slot<'gc>) -> ReturnCode;

#[repr(C)]
pub struct NativeProc {
    pub proc: Address,
}

fn native_proc_header_word(is_k: bool) -> u64 {
    let class_id = if is_k {
        builtin_class_ids::NATIVE_CONTINUATION
    } else {
        builtin_class_ids::NATIVE_PROCEDURE
    };

    class_header_word(ClassId::new(class_id).expect("builtin class id is nonzero"))
}

impl NativeProc {
    pub fn new<'gc>(ctx: MutationContext<'gc>, proc: Address, is_k: bool) -> Gc<'gc, Self> {
        Gc::new_with_header_word(ctx, NativeProc { proc }, native_proc_header_word(is_k))
    }
}

// SAFETY: NativeProc contains no GC-managed fields, `proc` is a raw function pointer.
// No tracing or weak processing is needed.
unsafe impl ClassTagged for NativeProc {
    const CLASS_IDS: &'static [u32] = &[
        crate::heap::object::builtin_class_ids::NATIVE_PROCEDURE,
        crate::heap::object::builtin_class_ids::NATIVE_CONTINUATION,
    ];
    const TYPE_NAME: &'static str = "native-procedure";
}

// SAFETY: GC trace for `NativeProc`, all reachable heap fields are visited
unsafe impl Trace for NativeProc {
    // SAFETY: No GC references in NativeProc, nothing to trace.
    unsafe fn trace(&mut self, _vis: &mut Visitor) {}

    // SAFETY: No weak references in NativeProc.
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::heap::WeakProcessor) {}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum ReturnCode {
    ReturnOk = 0,
    ReturnErr = 1,
    /// Non-continuable raise from a thunk, trampoline re-enters the exception handler.
    Raise = 2,
    Continue = 3,
    /// One-shot bail
    ResumeTemplate = 4,
    /// One value, written in place at `sfp[2]`: return it to the caller.
    ReturnOne = 5,
    /// The native rewrote its own frame into a call: `fp[1]` is the closure
    /// to enter, its `Tc::ac` arguments sit in `fp[2..]`, and `Tc::stack.fp`
    /// names the frame (the stack may have moved). The machine `subr_call`
    /// jumps straight into the callee, the driver takes `Work::Enter`.
    EnterBody = 6,
}

#[repr(C)]
pub struct NativeReturn<'gc> {
    pub code: ReturnCode,
    pub value: Value<'gc>,
}
