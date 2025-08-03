use std::{cell::Cell, sync::OnceLock};

use rsgc::{Gc, Global, Rootable, Trace, mmtk::util::Address};

use crate::{
    jit::trampoline::TRAMPOLINES,
    runtime::{
        Context,
        value::{Closure, ScmHeader, TypeCode16, Value},
    },
};

pub mod primitive;
pub mod require;

#[repr(C)]
pub struct VMState<'gc> {
    pub proc: Value<'gc>,
    pub argc: Cell<usize>,
    pub argv: Address,

    pub args_start: Address,
    pub args_size: usize,
}
unsafe impl<'gc> Trace for VMState<'gc> {
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}

    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        visitor.trace(&mut self.proc);

        for i in 0..self.argc.get() {
            unsafe {
                let addr = self.argv.sub(i * size_of::<Value>()).as_mut_ref::<Value>();
                visitor.trace(addr);
            }
        }
    }
}

const ARGS_SIZE: usize = 8192;

impl<'gc> VMState<'gc> {
    pub fn new() -> Self {
        unsafe {
            let args_start = libc::mmap(
                std::ptr::null_mut(),
                ARGS_SIZE * size_of::<Value>(),
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            );

            if args_start == libc::MAP_FAILED {
                panic!("Failed to allocate memory for VMState arguments");
            }

            let args_start = Address::from_ptr(args_start as *mut u8);
            let args_size = ARGS_SIZE * size_of::<Value>();
            let argv = args_start;

            VMState {
                proc: Value::undefined(),
                argc: Cell::new(0),
                argv,

                args_start: args_start,
                args_size: args_size,
            }
        }
    }

    pub fn argument_count(&self) -> usize {
        self.argc.get()
    }

    pub fn arguments(&self) -> &[Value<'gc>] {
        unsafe { std::slice::from_raw_parts(self.argv.to_ptr::<Value>(), self.argc.get()) }
    }

    pub(crate) fn push_arg(&self, arg: Value<'gc>) {
        if self.argc.get() >= 8192 {
            panic!("Too many arguments passed to VMState");
        }
        unsafe {
            let addr = self.argv.add(self.argc.get() * size_of::<Value>());
            addr.store(arg);
        }
        self.argc.set(self.argc.get() + 1);
    }

    #[allow(dead_code)]
    pub(crate) fn reset_args(&mut self) {
        self.argc.set(0);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum VMReturnCode {
    Error = 0,
    Return = 1,

    NonProcedureCall,
    /// Mismatched arguments.
    ///
    /// Expects return value to be an integer indicating the number of arguments expected where
    /// negative integer means the number of arguments is at least the absolute value of the integer.
    ArgumentMismatch,
    Yield,
    Throw,
    ThrowValue,
    /// Expects `VMReturn::value` to be a cons of `value` and `data`.
    ThrowValueAndData,
}

#[repr(C)]
pub struct VMReturn<'gc> {
    pub code: VMReturnCode,
    pub value: Value<'gc>,
}

impl<'gc> VMReturn<'gc> {
    pub fn new(code: VMReturnCode, value: Value<'gc>) -> Self {
        VMReturn { code, value }
    }

    pub fn is_error(&self) -> bool {
        self.code == VMReturnCode::Error
    }

    pub fn is_return(&self) -> bool {
        self.code == VMReturnCode::Return
    }

    pub fn ok(value: Value<'gc>) -> Self {
        VMReturn::new(VMReturnCode::Return, value)
    }

    pub fn error(val: Value<'gc>) -> Self {
        VMReturn::new(VMReturnCode::Error, val)
    }

    pub fn non_procedure_call(proc: Value<'gc>) -> Self {
        VMReturn::new(VMReturnCode::NonProcedureCall, proc)
    }

    pub fn throw(value: Value<'gc>) -> Self {
        VMReturn::new(VMReturnCode::Throw, value)
    }

    pub fn throw_value_and_data(key_and_subr_and_messages: Value<'gc>) -> Self {
        VMReturn::new(VMReturnCode::ThrowValueAndData, key_and_subr_and_messages)
    }

    pub fn throw_value(value: Value<'gc>) -> Self {
        VMReturn::new(VMReturnCode::ThrowValue, value)
    }

    pub fn yield_value(value: Value<'gc>) -> Self {
        VMReturn::new(VMReturnCode::Yield, value)
    }
}

static EXIT_CONTINUATION: OnceLock<Global<Rootable!(Value<'_>)>> = OnceLock::new();

pub fn exit_continuation<'gc>(ctx: Context<'gc>) -> Value<'gc> {
    *EXIT_CONTINUATION
        .get_or_init(|| {
            let code = TRAMPOLINES.exit_continuation();
            let closure = Gc::new(
                &ctx,
                Closure {
                    code: Address::from_ptr(code as *const ()),
                    header: ScmHeader::with_type_bits(TypeCode16::CLOSURE_K.bits() as _),
                    free: Value::null(),
                    meta: Value::new(false),
                },
            );

            Global::new(closure.into())
        })
        .fetch(&ctx)
}
