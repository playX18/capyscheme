//! Native procedure ABI values.

use super::*;

pub enum Return<'gc> {
    Continue(Value<'gc>),
    Err(Value<'gc>),
}

pub type NativeFn<'gc> = extern "C-unwind" fn(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
    retk: Value<'gc>,
) -> NativeReturn<'gc>;

pub type NativeContinuation<'gc> = extern "C-unwind" fn(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
) -> NativeReturn<'gc>;

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

    class_header_word(ClassId::new(class_id).unwrap())
}

impl NativeProc {
    pub fn new<'gc>(ctx: Context<'gc>, proc: Address, is_k: bool) -> Gc<'gc, Self> {
        Gc::new_with_header_word(*ctx, NativeProc { proc }, native_proc_header_word(is_k))
    }
}

// SAFETY: NativeProc contains no GC-managed fields — `proc` is a raw function pointer.
// No tracing or weak processing is needed.
unsafe impl ClassTagged for NativeProc {
    const CLASS_IDS: &'static [u32] = &[
        crate::rsgc::object::builtin_class_ids::NATIVE_PROCEDURE,
        crate::rsgc::object::builtin_class_ids::NATIVE_CONTINUATION,
    ];
    const TYPE_NAME: &'static str = "native-procedure";
}

unsafe impl Trace for NativeProc {
    // SAFETY: No GC references in NativeProc; nothing to trace.
    unsafe fn trace(&mut self, _vis: &mut Visitor) {}

    // SAFETY: No weak references in NativeProc.
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum ReturnCode {
    ReturnOk = 0,
    ReturnErr = 1,
    Continue = 3,
}

#[repr(C)]
pub struct NativeReturn<'gc> {
    pub code: ReturnCode,
    pub value: Value<'gc>,
}
