//! Stack frame layout for CapyScheme interpreter/JIT compiler.

use std::marker::PhantomData;

use crate::prelude::Value;

#[repr(C)]
pub struct CallerFrameAndPC {
    pub caller_frame: *mut CallFrame,
    pub return_pc: *const u8,
}

impl CallerFrameAndPC {
    pub const SIZE_IN_REGISTERS: usize = 2 * size_of::<usize>() / size_of::<usize>();
}

///  Layout of CallFrame
///
///  Higher addresses are on top.
///  Slots marked with "(+)" have a different meaning when executing Wasm code.
///  See details below the diagram.
///```text
///
///   |          ......            |   |
///   +----------------------------+   |
///   |           argN             |   v  lower addresses
///   +----------------------------+
///   |           ...              |
///   +----------------------------+
///   |           arg1             |
///   +----------------------------+
///   |           arg0             |
///   +----------------------------+
///   |       argumentCount        |
///   +----------------------------+
///   |          callee            |
///   +----------------------------+
///   |       code_block           |
///   +----------------------------+
///   |       returnAddress        |
///   +----------------------------+
///   |        callerFrame         |  <- callee's cfr is pointing at this address
///   +----------------------------+
///   |          local0            |
///   +----------------------------+
///   |          local1            |
///   +----------------------------+
///   |           ...              |
///   +----------------------------+
///   |          localN            |
///   +----------------------------+
///   |          ......            |
///```

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(isize)]
pub enum CallFrameSlot {
    CodeBlock = CallerFrameAndPC::SIZE_IN_REGISTERS as isize,
    Callee = Self::CodeBlock as isize + 1,
    ArgumentCount = Self::Callee as isize + 1,
    FirstArgument = Self::ArgumentCount as isize + 1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]

pub struct CallFrame(PhantomData<u64>);

impl CallFrame {
    pub const fn argument_offset(argument: usize) -> isize {
        CallFrameSlot::FirstArgument as isize + argument as isize
    }

    pub fn argument_count(&self) -> usize {
        unsafe { self.slot(CallFrameSlot::ArgumentCount).read() as u32 as usize }
    }

    pub unsafe fn argument_unchecked<'gc>(&self, argument: usize) -> Value<'gc> {
        unsafe {
            let addr = self.slot(CallFrameSlot::FirstArgument).cast::<Value<'gc>>();
            let offset = Self::argument_offset(argument) as usize;
            addr.add(offset).read()
        }
    }

    pub fn argument<'gc>(&self, argument: usize) -> Value<'gc> {
        if argument < self.argument_count() {
            unsafe { self.argument_unchecked(argument) }
        } else {
            Value::undefined()
        }
    }

    pub fn set_argument<'gc>(&mut self, argument: usize, value: Value<'gc>) {
        if argument < self.argument_count() {
            unsafe { self.set_argument_unchecked(argument, value) }
        }
    }

    pub unsafe fn set_argument_unchecked<'gc>(&mut self, argument: usize, value: Value<'gc>) {
        unsafe {
            let addr = self
                .slot_mut(CallFrameSlot::FirstArgument)
                .cast::<Value<'gc>>();
            let offset = Self::argument_offset(argument) as usize;
            addr.add(offset).write(value);
        }
    }

    unsafe fn slot(&self, slot: CallFrameSlot) -> *const u64 {
        unsafe {
            let addr = self as *const Self as *const u8;
            let slot_addr = addr.add(slot as usize * size_of::<u64>());
            slot_addr.cast::<u64>()
        }
    }

    unsafe fn slot_mut(&mut self, slot: CallFrameSlot) -> *mut u64 {
        unsafe {
            let addr = self as *mut Self as *mut u8;
            let slot_addr = addr.add(slot as usize * size_of::<u64>());
            slot_addr.cast::<u64>()
        }
    }
}

pub union Register<'gc> {
    pub as_i64: i64,
    pub as_u64: u64,
    pub as_f64: f64,
    pub ptr: *mut (),
    pub val: Value<'gc>,
}
