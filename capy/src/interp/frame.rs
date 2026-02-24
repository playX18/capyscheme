//! ActivationFrame representation and management.
//!
//! Even though we use Continuation-Passing Style (CPS) for function calls in the interpreter,
//! we still need to maintain at least one ActivationFrame per function activation to hold local variables
//! and other metadata. This module defines the ActivationFrame structure and provides utilities for
//! creating, managing, and accessing ActivationFrames during interpretation.

use std::marker::PhantomData;

use crate::{
    Gc,
    bytecode::code_block::CodeBlock,
    prelude::{Value, Vector},
};

#[repr(C)]
pub struct ActivationFrame<'gc> {
    marker: PhantomData<&'gc ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(isize)]
pub enum FrameSlot {
    CodeBlock = 0,
    Callee = Self::CodeBlock as isize + 1,
    ArgumentCount = Self::Callee as isize + 1,
    FirstArgument = Self::ArgumentCount as isize + 1,
}

impl<'gc> ActivationFrame<'gc> {
    pub fn mut_slots(&mut self) -> *mut Slot<'gc> {
        (self as *mut ActivationFrame<'gc>).cast()
    }

    pub fn slots(&self) -> *const Slot<'gc> {
        (self as *const ActivationFrame<'gc>).cast()
    }

    pub fn callee(&self) -> Value<'gc> {
        unsafe { self.slots().offset(FrameSlot::Callee as isize).read().value }
    }

    pub fn code_block(&self) -> Gc<'gc, CodeBlock<'gc>> {
        unsafe { self.slots().offset(FrameSlot::CodeBlock as isize).read().cb }
    }

    pub fn constant(&self, ix: usize) -> Value<'gc> {
        let cb = self.code_block();
        cb.constants.downcast::<Vector>()[ix].get()
    }

    pub fn argument(&self, ix: usize) -> Value<'gc> {
        unsafe {
            self.slots()
                .offset(FrameSlot::FirstArgument as isize + ix as isize)
                .read()
                .value
        }
    }

    pub fn arguments(&self) -> &[Value<'gc>] {
        let argc = self.argument_count();
        unsafe {
            std::slice::from_raw_parts(
                self.slots()
                    .offset(FrameSlot::FirstArgument as isize)
                    .cast(),
                argc,
            )
        }
    }

    pub fn local(&self, ix: usize) -> Value<'gc> {
        unsafe { self.slots().sub(ix + 1).read().value }
    }

    pub fn set_local(&mut self, ix: usize, val: Value<'gc>) {
        unsafe {
            self.mut_slots().sub(ix + 1).write(Slot { value: val });
        }
    }

    pub fn local_slot(&mut self, ix: usize) -> *mut Slot<'gc> {
        unsafe { self.mut_slots().sub(ix + 1) }
    }

    pub fn argument_count(&self) -> usize {
        unsafe {
            self.slots()
                .offset(FrameSlot::ArgumentCount as isize)
                .read()
                .u4 as usize
        }
    }

    pub fn set_argument(&mut self, ix: usize, val: Value<'gc>) {
        unsafe {
            self.mut_slots()
                .offset(FrameSlot::FirstArgument as isize + ix as isize)
                .write(Slot { value: val });
        }
    }

    pub fn set_callee(&mut self, val: Value<'gc>) {
        unsafe {
            self.mut_slots()
                .offset(FrameSlot::Callee as isize)
                .write(Slot { value: val });
        }
    }

    pub fn set_code_block(&mut self, cb: Gc<'gc, CodeBlock<'gc>>) {
        unsafe {
            self.mut_slots()
                .offset(FrameSlot::CodeBlock as isize)
                .write(Slot { cb });
        }
    }

    /// Find the frame slot where the arguments start and frame itself starts.
    ///
    /// Frame "end" is known by interpreter as `sp` pointer.
    pub fn start(&mut self) -> *mut Slot<'gc> {
        let argc = self.argument_count();
        unsafe {
            self.mut_slots()
                .offset(FrameSlot::FirstArgument as isize + argc as isize)
        }
    }
}

pub union Slot<'gc> {
    pub value: Value<'gc>,
    pub u8: u64,
    pub r8: f64,
    pub i4: i32,
    pub u4: u32,
    pub i2: i16,
    pub u2: u16,
    pub i1: i8,
    pub u1: u8,
    pub cb: Gc<'gc, CodeBlock<'gc>>,
}
