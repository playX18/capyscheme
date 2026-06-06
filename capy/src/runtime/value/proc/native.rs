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

static NATIVE_PROC_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    VTableOf::<'static, NativeProc>::VT,
    TypeCode16::NATIVE_PROC.bits(),
);
pub static NATIVE_PROC_INFO: &HeapTypeInfo = &NATIVE_PROC_INFO_VALUE;

static NATIVE_K_INFO_VALUE: HeapTypeInfo = HeapTypeInfo::new(
    VTableOf::<'static, NativeProc>::VT,
    TypeCode16::NATIVE_K.bits(),
);
pub static NATIVE_K_INFO: &HeapTypeInfo = &NATIVE_K_INFO_VALUE;

impl NativeProc {
    pub fn new<'gc>(ctx: Context<'gc>, proc: Address, is_k: bool) -> Gc<'gc, Self> {
        let tc = if is_k {
            NATIVE_K_INFO
        } else {
            NATIVE_PROC_INFO
        };

        Gc::new_with_info(*ctx, NativeProc { proc }, tc)
    }
}

// SAFETY: NativeProc contains no GC-managed fields — `proc` is a raw function pointer.
// No tracing or weak processing is needed.
unsafe impl Tagged for NativeProc {
    const TC8: TypeCode8 = TypeCode8::NATIVE_PROCEDURE;
    const TC16: &[TypeCode16] = &[TypeCode16::NATIVE_PROC, TypeCode16::NATIVE_K];
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
