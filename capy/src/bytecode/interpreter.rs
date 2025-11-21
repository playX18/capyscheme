//! Interpreter for CapyScheme bytecode.
//!
//! Instruction format is simplified due to the fact that we don't have "call-stack"
//! in the traditional sense due to CPS.
//!
//! Scheme code runs in a frame that looks like this:
//! ```text
//! +------------------+ <- fp (frame pointer)
//! |  self (Closure)  | ; Points to the current closure
//! +------------------+
//! |     retk         | ; Continuation to return to (optional, does not exist when calling into other continuations)
//! +------------------+
//! |     arg 0        |
//! +------------------+
//! |     arg 1        |
//! +------------------+
//! |      ...         |
//! +------------------+
//! |     arg N        |
//! +------------------+ <- sp (stack pointer)
//! ```
//!
//! To find number of arguments all you have to do is to calculate `sp - fp` and subtract 2 (for self and retk).
//!
//! Continuations follow the same layout, except `retk` is not present.
//!
//!
//! ## Calls
//!
//! There is only one way to call a function: using `return-call` opcode which shifts arguments from top of stack to the beginning
//! of the frame, sets `self` to the called closure and jumps to its code. `return-call` is universal in a sense
//! that it can invoke both normal functions and continuations.
//!
//! ## Exceptions
//!
//! Some interpreter instructions can raise exceptions. When that happens, the interpreter invokes `raise` procedure from
//! Scheme side with the error value as argument. The current continuation for `raise` is set to a continuatioon
//! that returns to the native code if `raise` fails.
//!
//! This has a consequence that `raise` can't capture real current continuation.
//!
//! ## Yielding
//!
//! When GC is requested or blocking I/O is performed, the interpreter yields back to native code
//! to handle the request. When the operation is complete, the interpreter resumes execution. Yield
//! may only happen at `enter`, `loop-hint` and `native-call` opcodes.
//!
//!
//! ## Native calls
//!
//! Native calls are handled by `native-call` opcode

use crate::{
    prelude::{Closure, ClosureRef, Number, Value},
    runtime::{
        Context,
        vm::{VMResult, exceptions::ConditionBuilder},
    },
};
use mmtk::util::Address;
use std::sync::atomic::Ordering;

#[inline(always)]
pub const fn unpack_8_8_8(op: u32) -> (u8, u8, u8) {
    let a = (op >> 8) & 0xff;
    let b = (op >> 16) & 0xff;
    let c = op >> 24;
    (a as u8, b as u8, c as u8)
}

#[inline(always)]
pub const fn unpack_8_16(op: u32) -> (u8, u16) {
    let a = (op >> 8) & 0xff;
    let b = op >> 16;
    (a as u8, b as u16)
}

#[inline(always)]
pub const fn unpack_12_12(op: u32) -> (u16, u16) {
    let a = (op >> 8) & 0xfff;
    let b = op >> 20;
    (a as u16, b as u16)
}

#[inline(always)]
pub const fn unpack_24(op: u32) -> u32 {
    op >> 8
}

#[inline(always)]
pub const fn unpack_8_24(op: u32) -> (u8, u32) {
    let a = op & 0xff;
    let b = op >> 8;
    (a as u8, b)
}

#[inline(always)]
pub const fn unpack_16_16(op: u32) -> (u16, u16) {
    let a = op & 0xffff;
    let b = op >> 16;
    (a as u16, b as u16)
}

/// An interpreter frame.
///
/// Only one frame per Scheme activation is active at a time. Recursive
/// activations may allocate multiple frames, but only one is active.
pub struct Frame<'gc> {
    /// Accumulator register.
    pub accumulator: Value<'gc>,
    /// Stack pointer.
    pub sp: *mut Value<'gc>,
    /// Frame pointer.
    pub fp: *mut Value<'gc>,
    /// Instruction pointer.
    pub ip: *const u8,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CompareFlag {
    None,
    Equal,
    LessThan,
    Invalid,
}

/// The result of executing the interpreter.
///
/// [`accumulator`](Frame::accumulator) stores the return value (be it return or error value).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum InterpreterResult<'gc> {
    /// Return from the current frame.
    Return(Value<'gc>),
    /// An error occurred.
    Error(Value<'gc>),
    /// Yield execution.
    Yield(Value<'gc>),
}

type InterpreterFn = for<'gc> fn(
    ctx: Context<'gc>,
    ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    accumulator: IValue<'gc>,
    cf: CompareFlag,
) -> InterpreterResult<'gc>;

#[derive(Copy, Clone)]
pub union IValue<'gc> {
    pub value: Value<'gc>,
    pub u64: u64,
    pub i32: i32,
    pub addr: Address,
}

impl<'gc> IValue<'gc> {
    pub fn value(&self) -> Value<'gc> {
        unsafe { self.value }
    }
}

impl<'gc> From<Value<'gc>> for IValue<'gc> {
    fn from(value: Value<'gc>) -> Self {
        IValue { value }
    }
}

#[allow(dead_code)]
fn nargs<'gc>(sp: *mut IValue<'gc>, fp: *mut IValue<'gc>) -> usize {
    unsafe { sp.offset_from(fp) as usize }
}

static HANDLERS: [InterpreterFn; 256] = [unused_op; 256];

macro_rules! next {
    ($ctx: expr, $ip: expr, $sp: expr, $fp: expr, $accumulator: expr, $cf: expr) => {{
        #[allow(unused)]
        let opcode = unsafe { $ip.read8() };
        let handler = HANDLERS[opcode as usize];
        handler($ctx, $ip, $sp, $fp, $accumulator, $cf)
    }};
}

/// First frame slot is always the closure.
#[inline(always)]
pub fn closure<'gc>(fp: *mut IValue<'gc>) -> ClosureRef<'gc> {
    unsafe { fp.read().value.downcast() }
}

macro_rules! pop {
    ($sp: ident) => {{
        #[allow(unused)]
        unsafe {
            $sp = $sp.sub(1);
            $sp.read()
        }
    }};
}

macro_rules! push {
    ($sp: ident, $value: expr) => {{
        #[allow(unused)]
        unsafe {
            $sp.write($value);
            $sp = $sp.add(1);
        }
    }};
}

pub fn unused_op<'gc>(
    _ctx: Context<'gc>,
    _ip: InstructionPointer,
    _sp: *mut IValue<'gc>,
    _fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    panic!("unimplemented opcode executed");
}

pub fn halt<'gc>(
    _ctx: Context<'gc>,
    _ip: InstructionPointer,
    _sp: *mut IValue<'gc>,
    _fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    InterpreterResult::Return(unsafe { _accumulator.value })
}

pub fn entry<'gc>(
    ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    accumulator: IValue<'gc>,
    cf: CompareFlag,
) -> InterpreterResult<'gc> {
    next!(ctx, ip, sp, fp, accumulator, cf)
}

pub fn const_nil<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    next!(_ctx, ip, sp, fp, Value::null().into(), _cf)
}

pub fn const_true<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    next!(_ctx, ip, sp, fp, Value::new(true).into(), _cf)
}

pub fn const_false<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    next!(_ctx, ip, sp, fp, Value::new(false).into(), _cf)
}

pub fn const_fixnum<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let fixnum = ip.read32() as i32;
    next!(_ctx, ip, sp, fp, Value::new(fixnum).into(), _cf)
}

pub fn constant<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let _index = ip.read32();
    // Placeholder: load constant from constant pool
    let constant = Value::null(); // Replace with actual constant lookup
    next!(_ctx, ip, sp, fp, constant.into(), _cf)
}

pub fn load<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let spoff = ip.read16() as isize;
    let value = unsafe { sp.offset(spoff).read() };
    next!(_ctx, ip, sp, fp, value, _cf)
}

pub fn store<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let spoff = ip.read16() as isize;
    unsafe {
        sp.offset(spoff).write(_accumulator);
    }
    next!(_ctx, ip, sp, fp, _accumulator, _cf)
}

pub fn pop<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let new_sp = unsafe { sp.sub(1) };
    let new_acc = unsafe { new_sp.read() };
    next!(_ctx, ip, new_sp, fp, new_acc, _cf)
}

pub fn push<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    unsafe {
        sp.write(_accumulator);
    }
    let new_sp = unsafe { sp.add(1) };
    next!(_ctx, ip, new_sp, fp, _accumulator, _cf)
}

pub fn dup<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    mut sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    unsafe {
        let top = sp.sub(1).read();
        sp.write(top);
        sp = sp.add(1);
    }
    next!(_ctx, ip, sp, fp, _accumulator, _cf)
}

pub fn popn<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let n = ip.read16() as usize;
    let new_sp = unsafe { sp.sub(n) };
    next!(_ctx, ip, new_sp, fp, _accumulator, _cf)
}

pub fn j<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let offset = ip.read32() as i32;
    ip.offset(offset);
    next!(_ctx, ip, sp, fp, _accumulator, _cf)
}

pub fn jlt<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let offset = ip.read32() as i32;
    if cf == CompareFlag::LessThan {
        ip.offset(offset);
    }
    next!(_ctx, ip, sp, fp, _accumulator, cf)
}

pub fn jle<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let offset = ip.read32() as i32;
    if cf == CompareFlag::LessThan || cf == CompareFlag::Equal {
        ip.offset(offset);
    }
    next!(_ctx, ip, sp, fp, _accumulator, cf)
}

pub fn je<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let offset = ip.read32() as i32;
    if cf == CompareFlag::Equal {
        ip.offset(offset);
    }
    next!(_ctx, ip, sp, fp, _accumulator, cf)
}

pub fn jnl<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let offset = ip.read32() as i32;
    if cf != CompareFlag::LessThan {
        ip.offset(offset);
    }
    next!(_ctx, ip, sp, fp, _accumulator, cf)
}

pub fn jne<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let offset = ip.read32() as i32;
    if cf != CompareFlag::Equal {
        ip.offset(offset);
    }
    next!(_ctx, ip, sp, fp, _accumulator, cf)
}

pub fn jge<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let offset = ip.read32() as i32;
    if cf == CompareFlag::None {
        ip.offset(offset);
    }
    next!(_ctx, ip, sp, fp, _accumulator, cf)
}

pub fn jnge<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let offset = ip.read32() as i32;
    if cf != CompareFlag::None {
        ip.offset(offset);
    }
    next!(_ctx, ip, sp, fp, _accumulator, cf)
}

pub fn immediatep<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    next!(
        _ctx,
        ip,
        sp,
        fp,
        _accumulator,
        if unsafe { _accumulator.value.is_immediate() } {
            CompareFlag::Equal
        } else {
            CompareFlag::None
        }
    )
}

pub fn heap_objectp<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    next!(
        _ctx,
        ip,
        sp,
        fp,
        _accumulator,
        if unsafe { !_accumulator.value.is_immediate() } {
            CompareFlag::Equal
        } else {
            CompareFlag::None
        }
    )
}

pub fn tc8eq<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let typ8 = ip.read8();
    next!(
        _ctx,
        ip,
        sp,
        fp,
        _accumulator,
        if unsafe { _accumulator.value.typ8().bits() == typ8 } {
            CompareFlag::Equal
        } else {
            CompareFlag::None
        }
    )
}

pub fn tc16eq<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let typ16 = ip.read16();
    next!(
        _ctx,
        ip,
        sp,
        fp,
        _accumulator,
        if unsafe { _accumulator.value.typ16().bits() == typ16 } {
            CompareFlag::Equal
        } else {
            CompareFlag::None
        }
    )
}

pub fn eq<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    mut sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let operand = pop!(sp);
    next!(
        _ctx,
        ip,
        sp,
        fp,
        _accumulator,
        if unsafe { _accumulator.value == operand.value } {
            CompareFlag::Equal
        } else {
            CompareFlag::None
        }
    )
}

pub fn eqv<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    mut sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let operand = pop!(sp);
    next!(
        _ctx,
        ip,
        sp,
        fp,
        _accumulator,
        if unsafe { _accumulator.value.eqv(operand.value) } {
            CompareFlag::Equal
        } else {
            CompareFlag::None
        }
    )
}

pub fn equal<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    mut sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let operand = pop!(sp);
    next!(
        _ctx,
        ip,
        sp,
        fp,
        _accumulator,
        if unsafe {
            _accumulator
                .value
                .equal(operand.value, &mut Default::default())
        } {
            CompareFlag::Equal
        } else {
            CompareFlag::None
        }
    )
}

pub fn neq<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    mut sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    {
        let lhs = accumulator.value();
        let rhs = pop!(sp).value();

        let Some(lhs) = lhs.number() else {
            become raise(
                _ctx,
                ip,
                sp,
                fp,
                ConditionBuilder::new(_ctx)
                    .assertion()
                    .who("=")
                    .message("expected number as first argument")
                    .irritants(&[lhs])
                    .build()
                    .into(),
                _cf,
            )
        };

        let Some(rhs) = rhs.number() else {
            become raise(
                _ctx,
                ip,
                sp,
                fp,
                ConditionBuilder::new(_ctx)
                    .assertion()
                    .who("=")
                    .message("expected number as second argument")
                    .irritants(&[rhs])
                    .build()
                    .into(),
                _cf,
            )
        };

        next!(
            _ctx,
            ip,
            sp,
            fp,
            accumulator,
            if Number::equal(_ctx, lhs, rhs) {
                CompareFlag::Equal
            } else {
                CompareFlag::None
            }
        )
    }
}

pub fn lt<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    mut sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let lhs = accumulator.value();
    let rhs = pop!(sp).value();

    let Some(lhs) = lhs.number() else {
        become raise(
            _ctx,
            ip,
            sp,
            fp,
            ConditionBuilder::new(_ctx)
                .assertion()
                .who("<")
                .message("expected number as first argument")
                .irritants(&[lhs])
                .build()
                .into(),
            _cf,
        )
    };

    let Some(rhs) = rhs.number() else {
        become raise(
            _ctx,
            ip,
            sp,
            fp,
            ConditionBuilder::new(_ctx)
                .assertion()
                .who("<")
                .message("expected number as second argument")
                .irritants(&[rhs])
                .build()
                .into(),
            _cf,
        )
    };

    next!(
        _ctx,
        ip,
        sp,
        fp,
        accumulator,
        match Number::compare(_ctx, lhs, rhs) {
            Some(std::cmp::Ordering::Less) => CompareFlag::LessThan,
            Some(_) => CompareFlag::None,
            None => CompareFlag::Invalid,
        }
    )
}

pub fn make_closure<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    _accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let _index = ip.read32();
    // Placeholder: create closure from function at index
    let closure = Value::null(); // Replace with actual closure creation
    next!(_ctx, ip, sp, fp, closure.into(), _cf)
}

pub fn return_call<'gc>(
    _ctx: Context<'gc>,
    mut ip: InstructionPointer,
    mut sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let nargs = ip.read16() as usize;

    if !accumulator.value().is::<Closure>() {
        become raise(
            _ctx,
            ip,
            sp,
            fp,
            ConditionBuilder::new(_ctx)
                .assertion()
                .message("attempt to invoke non-procedure")
                .irritants(&[accumulator.value()])
                .build()
                .into(),
            _cf,
        );
    }

    unsafe {
        let clos = accumulator.value().downcast::<Closure>();
        let code = clos.code.to_ptr::<u8>();

        // Shift arguments down to frame pointer
        // fp[0] = closure (replaces old self)
        // fp[1..] = arguments
        // fp[1] might be retk if calling a normal function
        fp.write(IValue { value: clos.into() });

        // Move arguments from current stack position down to fp+1
        let args_start = sp.sub(nargs);
        std::ptr::copy(args_start, fp.add(1), nargs);

        // Update stack pointer to point after the arguments
        sp = fp.add(1 + nargs);

        // Jump to the new closure's code
        let mut new_ip = InstructionPointer(code);
        become HANDLERS[new_ip.read8() as usize](
            _ctx,
            new_ip,
            sp,
            fp,
            IValue {
                value: Value::undefined(),
            },
            CompareFlag::None,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InstructionPointer(*const u8);

impl InstructionPointer {
    pub fn advance(&mut self, n: usize) {
        unsafe {
            self.0 = self.0.add(n);
        }
    }

    pub fn offset(&mut self, n: i32) {
        unsafe {
            self.0 = self.0.offset(n as isize);
        }
    }

    pub fn get(&self) -> *const u8 {
        self.0
    }

    pub fn read8(&mut self) -> u8 {
        unsafe {
            let byte = *self.0;
            self.0 = self.0.add(1);
            byte
        }
    }

    pub fn read16(&mut self) -> u16 {
        unsafe {
            let bytes = *(self.0 as *const u16);
            self.0 = self.0.add(2);
            u16::from_le(bytes)
        }
    }

    pub fn read32(&mut self) -> u32 {
        unsafe {
            let bytes = *(self.0 as *const u32);
            self.0 = self.0.add(4);
            u32::from_le(bytes)
        }
    }

    pub fn peek8(&self) -> u8 {
        unsafe { *self.0 }
    }

    pub fn peek16(&self) -> u16 {
        unsafe { u16::from_le(*(self.0 as *const u16)) }
    }

    pub fn peek32(&self) -> u32 {
        unsafe { u32::from_le(*(self.0 as *const u32)) }
    }
}

/// Raise an error with the given accumulator value.
///
/// This function performs call into `raise` on Scheme side
/// with current continuation set to continuation that returns
/// to the VM (if `raise` returns).
#[allow(unused)]
fn raise<'gc>(
    ctx: Context<'gc>,
    _ip: InstructionPointer,
    mut sp: *mut IValue<'gc>,
    fp: *mut IValue<'gc>,
    accumulator: IValue<'gc>,
    _cf: CompareFlag,
) -> InterpreterResult<'gc> {
    let raise_proc = ctx.public_ref("capy", "raise").expect("pre-boot error"); // => no boot module, not yet loaded

    unsafe {
        if !raise_proc.is::<Closure>() {
            panic!("pre-boot error: {}", accumulator.value);
        }
        sp = fp;

        push!(sp, raise_proc.into()); // procedure to call
        push!(sp, Value::undefined().into()); // retk placeholder
        push!(sp, accumulator); // exn

        let clos = raise_proc.downcast::<Closure>();
        become HANDLERS[clos.code.load::<u8>() as usize](
            ctx,
            InstructionPointer(clos.code.to_ptr::<u8>()),
            sp,
            fp,
            IValue {
                value: Value::undefined(),
            },
            CompareFlag::None,
        )
    }
}

/// Call into Scheme code.
pub fn call_into_scheme<'gc>(
    ctx: Context<'gc>,
    proc: ClosureRef<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> VMResult<'gc> {
    let old_runstack = ctx.state().runstack.get();

    let fp = old_runstack;
    let mut sp = old_runstack;

    const VSIZE: usize = size_of::<IValue>();
    unsafe {
        // TODO: set up retk properly
        let retk = Value::undefined();
        sp.store(proc);
        sp = sp.add(VSIZE);
        sp.store(retk);
        sp = sp.add(VSIZE);

        for arg in args {
            sp.store(arg);
            sp = sp.add(VSIZE);
        }
    }

    ctx.state().runstack.set(sp);
    // Increase nest level: when >= 1 we're in Scheme code, if >= 2 then
    // yielding is prohibited.
    ctx.state().nest_level.fetch_add(1, Ordering::Relaxed);

    let code = proc.code.to_ptr::<u8>();
    let mut ip = InstructionPointer(code);
    let result = HANDLERS[ip.read8() as usize](
        ctx,
        ip,
        sp.to_mut_ptr::<IValue>(),
        fp.to_mut_ptr::<IValue>(),
        IValue {
            value: Value::undefined(),
        },
        CompareFlag::None,
    );
    ctx.state().runstack.set(old_runstack);

    match result {
        InterpreterResult::Return(val) => VMResult::Ok(val),
        InterpreterResult::Error(err) => VMResult::Err(err),
        InterpreterResult::Yield(_) => VMResult::Yield,
    }
}
