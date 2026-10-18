//! One frame vocabulary for every engine: [`CallFrame`].
//!
//! A frame is laid out as follows:
//!
//! ```text
//!   fp[0]   return address: a machine address with a header before it
//!   fp[1]   the callee's closure
//!   fp[2..] arguments, then locals
//!   fp[-1]  the caller's bytecode return point, meaningful exactly when
//!           fp[0] is the interpreter's return glue
//! ```
//!

use core::mem::{align_of, size_of};
use std::ops::{Index, IndexMut};

use crate::bytecode::def::Register;
use crate::heap::Gc;
use crate::runtime::MutationContext;
use crate::runtime::interp::stack::{Ip, Slot};
use crate::runtime::sabi::Ra;
use crate::runtime::value::{Closure, CodeBlock, Value, Vector};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallFrameSlot {
    ReturnAddress = 0,
    Callee = 1,
    FirstArgument = 2,
}

/// A live Scheme frame: `fp`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(transparent)]
pub struct CallFrame<'gc>(Slot<'gc>);

impl<'gc> Index<Register> for CallFrame<'gc> {
    type Output = Value<'gc>;

    fn index(&self, index: Register) -> &Self::Output {
        unsafe { &*self.register(index) }
    }
}

impl<'gc> IndexMut<Register> for CallFrame<'gc> {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        unsafe { &mut *self.register(index) }
    }
}

const _: () = {
    assert!(size_of::<CallFrame<'_>>() == size_of::<Slot<'_>>());
    assert!(align_of::<CallFrame<'_>>() == align_of::<Slot<'_>>());
};

impl<'gc> CallFrame<'gc> {
    /// Wrap the frame pointer `slot`.
    #[inline(always)]
    pub fn create(slot: Slot<'gc>) -> Self {
        Self(slot)
    }

    #[inline(always)]
    pub fn slot(self) -> Slot<'gc> {
        self.0
    }

    /// The frame `slots` above this one.
    #[inline(always)]
    pub fn add(self, slots: usize) -> Self {
        Self(self.0.wrapping_add(slots))
    }

    /// The frame `slots` below this one.
    #[inline(always)]
    pub fn sub(self, slots: usize) -> Self {
        Self(self.0.wrapping_sub(slots))
    }

    /// The return address in slot 0.
    ///
    /// # Safety
    ///
    /// `self` is a live frame, whose slot 0 holds a return address.
    #[inline(always)]
    pub unsafe fn return_address(self) -> Ra {
        unsafe { (*self.0).bits() as usize as Ra }
    }

    /// Store `ra` as the return address in slot 0.
    ///
    /// # Safety
    ///
    /// `self` is a writable frame, and `ra` is a marker from
    /// [`crate::runtime::interp::markers`].
    #[inline(always)]
    pub unsafe fn set_return_address(self, ra: Ra) {
        unsafe { *self.0 = Value::from_raw(ra as usize as u64) }
    }

    /// The bytecode return point of a caller: `fp[-1]`, the caller's
    /// reserved `r[base-1]` slot.
    ///
    /// # Safety
    ///
    /// `self` is a live frame whose slot 0 is `INTERP_RETURN` or a machine
    /// return address into JIT code (never a stub marker).
    #[inline(always)]
    pub unsafe fn resume_ip(self) -> Ip {
        unsafe { (*self.0.sub(1)).bits() as usize as Ip }
    }

    /// Store `ip` as the bytecode return point in `fp[-1]`.
    ///
    /// # Safety
    ///
    /// `self` is a writable frame whose slot 0 is (or is about to become)
    /// `INTERP_RETURN`, and `ip` is the first byte of a return point.
    #[inline(always)]
    pub unsafe fn set_resume_ip(self, ip: Ip) {
        unsafe { *self.0.sub(1) = Value::from_raw(ip as usize as u64) }
    }

    /// The running closure, from slot 1.
    ///
    /// # Safety
    ///
    /// `self` is a live frame.
    #[inline(always)]
    pub unsafe fn callee(self) -> Value<'gc> {
        unsafe { *self.0.add(CallFrameSlot::Callee as usize) }
    }

    /// Store the running closure in slot 1.
    ///
    /// # Safety
    ///
    /// `self` is a writable frame.
    #[inline(always)]
    pub unsafe fn set_callee(self, callee: Value<'gc>) {
        unsafe { *self.0.add(CallFrameSlot::Callee as usize) = callee }
    }

    /// Argument `index`, from slot `2 + index`.
    ///
    /// # Safety
    ///
    /// `self` is a live frame entered with more than `index` arguments.
    #[inline(always)]
    pub unsafe fn argument(self, index: usize) -> Value<'gc> {
        unsafe { *self.0.add(CallFrameSlot::FirstArgument as usize + index) }
    }

    /// Store argument `index` in slot `2 + index`.
    ///
    /// # Safety
    ///
    /// `self` is a writable frame with a slot `2 + index`.
    #[inline(always)]
    pub unsafe fn set_argument(self, index: usize, value: Value<'gc>) {
        unsafe { *self.0.add(CallFrameSlot::FirstArgument as usize + index) = value }
    }

    /// Raw pointer to the arguments' first slot: arguments for a call,
    /// values for a return.
    #[inline(always)]
    pub fn arguments_ptr(self) -> *mut Value<'gc> {
        self.0.wrapping_add(CallFrameSlot::FirstArgument as usize)
    }

    /// Raw pointer to register `index`'s home slot.
    #[inline(always)]
    pub fn register(self, index: Register) -> *mut Value<'gc> {
        self.0.wrapping_add(index.0 as usize)
    }

    /// The running closure's code block, through the callee slot.
    ///
    /// # Safety
    ///
    /// `self` is a live frame whose slot 1 holds a live closure.
    #[inline(always)]
    pub unsafe fn code_block(self) -> Gc<'gc, CodeBlock<'gc>> {
        unsafe { Closure::code_block(Closure::from_value(self.callee())) }
    }

    /// Record the frame for the runtime before calling out of the
    /// interpreter.
    ///
    /// # Safety
    ///
    /// `self` is the running frame, the collector reads `fp` and `sp`
    /// from where this publishes them.
    #[inline(always)]
    pub unsafe fn park(self, ctx: MutationContext<'gc>) {
        let stack = &ctx.tc().stack;
        stack.fp.set(self.slot());
        stack.sp.set(self.arguments_ptr());
    }

    /// Pack returned values
    ///
    /// # Safety
    ///
    /// `self` is a live frame holding `count` values from slot 2 on.
    #[inline(always)]
    pub unsafe fn exit_value(self, ctx: MutationContext<'gc>, count: usize) -> Value<'gc> {
        unsafe {
            match count {
                0 => Value::undefined(),
                1 => self.argument(0),
                _ => {
                    Vector::from_slice(ctx, std::slice::from_raw_parts(self.arguments_ptr(), count))
                        .into()
                }
            }
        }
    }
}
