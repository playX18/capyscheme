//! Bytecode interpreter for the Capy VM.
//!
//! # CPS Interpreter
//!
//! This is a **Continuation-Passing Style (CPS)** interpreter. There is only ever
//! ONE activation frame on the stack at a time. Continuations are heap-allocated
//! and passed explicitly as arguments - there is no call stack in the traditional sense.
//!
//! # Stack Layout
//!
//! The interpreter uses a downward-growing stack with a single `ActivationFrame`:
//!
//! ```text
//!       Higher Addresses
//!                        ┌─────────────────────────────┐ ◄── bp.start()
//!                        │        Argument N-1         │     bp + 3 + (N-1)
//!                        ├─────────────────────────────┤
//!                        │           ...               │
//!                        ├─────────────────────────────┤
//!                        │        Argument 1           │     bp + 4
//!                        ├─────────────────────────────┤
//!                        │        Argument 0           │     FrameSlot::FirstArgument (bp + 3)
//!                        ├─────────────────────────────┤
//!                        │     Argument Count          │     FrameSlot::ArgumentCount (bp + 2)
//!                        ├─────────────────────────────┤
//!                        │         Callee              │     FrameSlot::Callee        (bp + 1)
//!                        ├─────────────────────────────┤
//!                        │        CodeBlock            │     FrameSlot::CodeBlock     (bp + 0)
//!                        ├─────────────────────────────┤ ◄── bp (ActivationFrame pointer)
//!                        │        Local 0              │     bp.local(0) = bp - 0
//!                        ├─────────────────────────────┤
//!                        │        Local 1              │     bp.local(1) = bp - 1
//!                        ├─────────────────────────────┤
//!                        │           ...               │
//!                        ├─────────────────────────────┤
//!                        │    Evaluation Stack ...     │
//!                        │                             │
//!       Lower Addresses  └─────────────────────────────┘ ◄── sp (stack pointer)
//! ```
//!
//! # Return-Call (Tail Call) Operation
//!
//! Since CPS makes every call a tail call, `op_return_call` simply replaces
//! the current frame in-place with the new callee's frame.
//!
//! ## Before `op_return_call` (calling `new_func` with 2 args)
//!
//! ```text
//!       Higher Addresses
//!                        ┌─────────────────────────────┐ ◄── bp.start()
//!                        │   arg 2 (continuation k)    │     bp + 5
//!                        ├─────────────────────────────┤
//!                        │   arg 1                     │     bp + 4
//!                        ├─────────────────────────────┤
//!                        │   arg 0                     │     bp + 3
//!                        ├─────────────────────────────┤
//!                        │   argc = 3                  │     bp + 2
//!                        ├─────────────────────────────┤
//!                        │   current callee            │     bp + 1
//!                        ├─────────────────────────────┤
//!                        │   current CodeBlock         │     bp + 0
//!                        ├─────────────────────────────┤ ◄── bp
//!                        │   local 0                   │
//!                        ├─────────────────────────────┤
//!                        │   ...                       │
//!                        ├─────────────────────────────┤
//!                        │   new_arg 1                 │     ─┐
//!                        ├─────────────────────────────┤      │ args on eval stack
//!                        │   new_arg 0                 │     ─┘
//!                        ├─────────────────────────────┤
//!                        │   new_func (callee)         │     ◄── popped first
//!       Lower Addresses  └─────────────────────────────┘ ◄── sp
//! ```
//!
//! ## After `op_return_call` (new frame installed)
//!
//! ```text
//!       Higher Addresses
//!                        ┌─────────────────────────────┐ ◄── bp.start() (same address as before!)
//!                        │   new_arg 1                 │     bp + 4  ─┐ copied from
//!                        ├─────────────────────────────┤              │ eval stack
//!                        │   new_arg 0                 │     bp + 3  ─┘
//!                        ├─────────────────────────────┤
//!                        │   argc = 2                  │     bp + 2
//!                        ├─────────────────────────────┤
//!                        │   new_func (callee)         │     bp + 1
//!                        ├─────────────────────────────┤
//!                        │   new CodeBlock             │     bp + 0
//!                        ├─────────────────────────────┤ ◄── bp (updated)
//!                        │   (space for locals)        │
//!       Lower Addresses  └─────────────────────────────┘ ◄── sp (updated, = bp)
//! ```
//!
//! ## Algorithm Steps (from the code)
//!
//! ```text
//!   1. callee = pop!(sp)                    // get new function
//!   2. cb = callee.closure.meta.codeblock   // extract CodeBlock
//!   3. start = bp.start()                   // top of current frame
//!   4. new_sp = start - argc                // destination for new args
//!   5. copy(sp -> new_sp, argc slots)       // move args up
//!   6. sp = new_sp
//!   7. push!(sp, argc)                      // FrameSlot::ArgumentCount
//!   8. push!(sp, callee)                    // FrameSlot::Callee
//!   9. push!(sp, cb)                        // FrameSlot::CodeBlock
//!  10. bp = sp                              // new frame pointer
//!  11. dispatch(pc=0, cb)                   // start executing new function
//! ```
//!
//! The stack is always bounded to a single frame - O(1) space regardless of call depth.
//!
//! # Reentrance (Rust → Scheme → Rust → Scheme)
//!
//! When Rust code calls into Scheme (e.g., invoking a callback), we need a fresh
//! stack since the current one is in use. Options:
//!
//! ## Option 1: Vec of Stacks (Stack Pool)
//!
//! ```text
//!   ┌──────────────────────────────────────────────────────────────────┐
//!   │  Context                                                         │
//!   │  ┌────────────────────────────────────────────────────────────┐  │
//!   │  │ stacks: Vec<Stack>                                         │  │
//!   │  │  ┌─────────┐  ┌─────────┐  ┌─────────┐                     │  │
//!   │  │  │ Stack 0 │  │ Stack 1 │  │ Stack 2 │  ...                │  │
//!   │  │  │ (in use)│  │ (in use)│  │ (free)  │                     │  │
//!   │  │  └─────────┘  └─────────┘  └─────────┘                     │  │
//!   │  │       │             │                                      │  │
//!   │  │       ▼             ▼                                      │  │
//!   │  │   Scheme A      Scheme B                                   │  │
//!   │  │   calls Rust    (callback                                  │  │
//!   │  │                 from A)                                    │  │
//!   │  └────────────────────────────────────────────────────────────┘  │
//!   │  active_stack: usize = 1                                         │
//!   └──────────────────────────────────────────────────────────────────┘
//! ```
//!
//! - On reentry: `active_stack += 1`, reuse or allocate `stacks[active_stack]`
//! - On return:  `active_stack -= 1`
//! - Pro: Simple, cache-friendly if reentry depth is bounded
//! - Con: Need to track which stack is active
//!
//! ## Option 2: Segmented / Linked Stacks
//!
//! ```text
//!   Stack 0 (original)          Stack 1 (reentry)
//!   ┌─────────────────┐         ┌─────────────────┐
//!   │    Frame A      │         │    Frame B      │
//!   │  (suspended)    │         │   (active)      │
//!   │                 │    ┌───►│                 │
//!   │  prev: null     │    │    │  prev: ────────┐│
//!   └─────────────────┘    │    └─────────────────┘│
//!          ▲               │           ▲          │
//!          └───────────────┼───────────┘          │
//!                          │                      │
//!                          └──────────────────────┘
//! ```
//!
//! - Each stack segment has a `prev` pointer to the caller's stack
//! - Pro: Arbitrary reentry depth, only allocate when needed
//! - Con: Extra indirection, harder to manage lifetimes
//!
//! ## Option 3: Copy-Out / Copy-In
//!
//! On reentry, copy the current frame to heap, reset stack:
//!
//! ```text
//!   Before reentry:                After reentry:
//!   ┌─────────────────┐            ┌─────────────────┐
//!   │    Frame A      │   copy     │   (saved to     │
//!   │   (active)      │ ────────►  │    heap blob)   │
//!   └─────────────────┘            └─────────────────┘
//!                                          │
//!   Stack reset:                           │restore
//!   ┌─────────────────┐                    │on return
//!   │    Frame B      │◄───────────────────┘
//!   │   (fresh)       │
//!   └─────────────────┘
//! ```
//!
//! - Pro: Single stack, minimal memory
//! - Con: Copy overhead on every reentry/return

use crate::bytecode::opc::*;
use crate::interp::frame::{ActivationFrame, Slot};
use crate::prelude::*;
use crate::runtime::value::proc::Closure2;
use crate::{Gc, bytecode::code_block::CodeBlock};

/// Recursively defines handlers for each opcode.
///
/// Must be only invoked by `define_interpreter!`
#[doc(hidden)]
macro_rules! define_handlers {
    (<$($l: lifetime),*>
        $ret: ty; $pb: ty; ($($ctx_name: ident : $ctx_ty: ty),*); $op_name: ident => |$pc: ident, $pb_arg: ident, $op: ident| $b:block, $($rest:tt)*)
    => {
        paste::paste! {
            pub fn [<__handle_ $op_name _narrow>]<$($l),*>($pc: usize, $pb_arg: $pb, $(#[allow(unused)] mut $ctx_name: $ctx_ty),*) -> $ret {
                #[allow(unused)]
                let ($op, $pc) = [<$op_name : camel>]::decode_from_buffer::<0, _>(&$pb_arg, $pc);

                $b
            }

            pub fn [<__handle_ $op_name _wide16>]<$($l),*>($pc: usize, $pb_arg: $pb, $(#[allow(unused)] mut $ctx_name: $ctx_ty),*) -> $ret {
                #[allow(unused)]
                let ($op, $pc) = [<$op_name : camel>]::decode_from_buffer::<1, _>(&$pb_arg, $pc);
                $b
            }

            pub fn [<__handle_ $op_name _wide32>]<$($l),*>($pc: usize, $pb_arg: $pb, $(#[allow(unused)] mut $ctx_name: $ctx_ty),*) -> $ret {
                #[allow(unused)]
                let ($op, $pc) = [<$op_name : camel>]::decode_from_buffer::<2, _>(&$pb_arg, $pc);
                $b
            }

            define_handlers!(<$($l),*> $ret; $pb; ($($ctx_name : $ctx_ty),*); $($rest)*);
        }
    };
    (<$($l: lifetime),*> $ret: ty; $pb: ty; ($($ctx_name: ident : $ctx_ty: ty),*);) => {};
}

#[macro_export]
macro_rules! define_interpreter {
    (
        $dispatch : ident <$($l: lifetime),*> : tailcall,
        bytecode_buffer: $pb:ty,
        return_type: $ret:ty,
        context: ($($ctx_name:ident : $ctx_ty:ty),* $(,)?),
        handlers: {
            $($op_name: ident => fn ($pc: ident, $pb_arg: ident, $op: ident) $b: block),*
        }
    ) => {
        paste::paste! {
            type Handler = for<$($l),*> fn(usize, $pb, $($ctx_ty),*) -> $ret;

            fn __invalid_handler<$($l),*>(_pc: usize, _pb: $pb, $(_: $ctx_ty),*) -> $ret {
                panic!("Invalid opcode")
            }

            fn __handle_op_nop_narrow<$($l),*>(pc: usize, pb: $pb, $($ctx_name: $ctx_ty),*) -> $ret {
                become $dispatch(pc, pb, $($ctx_name),*)
            }

            fn __handle_op_nop_wide16<$($l),*>(pc: usize, pb: $pb, $($ctx_name: $ctx_ty),*) -> $ret {
                become $dispatch(pc, pb, $($ctx_name),*)
            }

            fn __handle_op_nop_wide32<$($l),*>(pc: usize, pb: $pb, $($ctx_name: $ctx_ty),*) -> $ret {
                become $dispatch(pc, pb, $($ctx_name),*)
            }

            define_handlers!(<$($l),*> $ret; $pb; ($($ctx_name : $ctx_ty),*); $($op_name => |$pc, $pb_arg, $op| $b,)*);

            fn decode_extended_narrow<$($l),*>(pc: usize, pb: $pb, $($ctx_name: $ctx_ty),*) -> $ret {
                let opcode = pb.read_u16(pc);
                let pc = pc + 2;
                become DISPATCH_TABLE_EXTENDED_NARROW[opcode as usize](pc, pb, $($ctx_name),*)
            }

            fn decode_wide16<$($l),*>(pc: usize, pb: $pb, $($ctx_name: $ctx_ty),*) -> $ret {
                let opcode = pb.read_u16(pc);
                let pc = pc + 2;
                become DISPATCH_TABLE_WIDE16[opcode as usize](pc, pb, $($ctx_name),*)
            }

            fn decode_wide32<$($l),*>(pc: usize, pb: $pb, $($ctx_name: $ctx_ty),*) -> $ret {
                let opcode = pb.read_u32(pc);
                let pc = pc + 4;
                become DISPATCH_TABLE_WIDE32[opcode as usize](pc, pb, $($ctx_name),*)
            }

            #[unsafe(no_mangle)]
            #[unsafe(link_section = ".interp_dispatch_tables")]
            static DISPATCH_TABLE_NARROW: [Handler; 256] = {
                let mut table = [__invalid_handler as Handler; 256];

                table[OpCode::OpNop as usize] = __handle_op_nop_narrow;

                $(
                    if OpCode::[<$op_name : camel>].fits_in_narrow() {
                        table[OpCode::[<$op_name : camel>] as usize] = [<__handle_ $op_name _narrow>];
                    }
                )*

                table[prefix::EXTENDED_NARROW as usize] = decode_extended_narrow;
                table[prefix::WIDE16 as usize] = decode_wide16;
                table[prefix::WIDE32 as usize] = decode_wide32;

                table
            };

            #[allow(unused)]
            #[unsafe(link_section = ".interp_dispatch_tables")]
            static DISPATCH_TABLE_EXTENDED_NARROW: [Handler; OpCode::COUNT] = {
                let mut table = [__invalid_handler as Handler; OpCode::COUNT];

                table[OpCode::OpNop as usize] = __handle_op_nop_narrow;

                $(
                    table[OpCode::[<$op_name : camel>] as usize] = [<__handle_ $op_name _narrow>];
                )*

                table
            };

            #[allow(unused)]
            #[unsafe(link_section = ".interp_dispatch_tables")]
            static DISPATCH_TABLE_WIDE16: [Handler; OpCode::COUNT] = {
                let mut table = [__invalid_handler as Handler; OpCode::COUNT];

                table[OpCode::OpNop as usize] = __handle_op_nop_wide16;

                $(
                    table[OpCode::[<$op_name : camel>] as usize] = [<__handle_ $op_name _wide16>];
                )*

                table
            };

            #[allow(unused)]
            #[unsafe(link_section = ".interp_dispatch_tables")]
            static DISPATCH_TABLE_WIDE32: [Handler; OpCode::COUNT] = {
                let mut table = [__invalid_handler as Handler; OpCode::COUNT];

                table[OpCode::OpNop as usize] = __handle_op_nop_wide32;

                $(
                    table[OpCode::[<$op_name : camel>] as usize] = [<__handle_ $op_name _wide32>];
                )*

                table
            };
        }

        #[inline(always)]
        pub fn $dispatch<$($l),*>(pc: usize, pb: $pb, $($ctx_name: $ctx_ty),*) -> $ret {
            let first_byte = pb.read_u8(pc);
            let pc = pc + 1;
            become DISPATCH_TABLE_NARROW[first_byte as usize](pc, pb, $($ctx_name),*)
        }
    };
    (
        $dispatch : ident : switch,
        bytecode_buffer: $pb:ty,
        return_type: $ret:ty,
        context: ($($ctx_name:ident : $ctx_ty:ty),* $(,)?),
        handlers: {
            $($op_name: ident => fn ($pc: ident, $pb_arg: ident, $op: ident) $b: block),*
        }
    ) => {
        paste::paste! {
            fn __invalid_handler(_pc: usize, _pb: $pb, $(_: $ctx_ty),*) -> $ret {
                panic!("Invalid opcode")
            }
        }
    };
}

macro_rules! pop {
    ($sp: expr) => {
        unsafe {
            let val = $sp.read().value;
            $sp = $sp.add(1);
            val
        }
    };
}

macro_rules! push {
    ($sp: expr, $val: expr) => {
        #[allow(unused)]
        unsafe {
            $sp = $sp.sub(1);
            $sp.write(Slot { value: $val });
        }
    };
    (@slot $sp: expr, $val: expr) => {
        #[allow(unused)]
        unsafe {
            $sp = $sp.sub(1);
            $sp.write($val);
        }
    };
}

macro_rules! reserve {
    ($sp: expr, $n: expr) => {
        #[allow(unused)]
        unsafe {
            $sp = $sp.sub($n);
        }
    };
}

define_interpreter! {
    dispatch<'gc>: tailcall,
    bytecode_buffer: Gc<'gc, CodeBlock<'gc>>,
    return_type: (),
    context: (ctx: Context<'gc>, bp: &'gc mut ActivationFrame<'gc>, sp: *mut Slot<'gc>),
    handlers: {
        op_enter =>
        fn (pc, pb, op) {
            /*
                enter function/continuation: allocate space for locals,
                check for JIT compilation, etc.
             */
            let cb = bp.code_block();
            let nlocals = cb.nlocals as usize;
            reserve!(sp, nlocals);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldarg0 =>
        fn (pc, pb, op) {
            let arg = bp.argument(0);
            push!(sp, arg);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldarg1 =>
        fn (pc, pb, op) {
            let arg = bp.argument(1);
            push!(sp, arg);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldarg2 =>
        fn (pc, pb, op) {
            let arg = bp.argument(2);
            push!(sp, arg);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldarg3 =>
        fn (pc, pb, op) {
            let arg = bp.argument(3);
            push!(sp, arg);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldarg4 =>
        fn (pc, pb, op) {
            let arg = bp.argument(4);
            push!(sp, arg);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldarg =>
        fn (pc, pb, op) {
            let arg = bp.argument(op.index as usize);
            push!(sp, arg);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldloc0 =>
        fn (pc, pb, op) {
            let local = bp.local(0);
            push!(sp, local);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldloc1 =>
        fn (pc, pb, op) {
            let local = bp.local(1);
            push!(sp, local);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldloc2 =>
        fn (pc, pb, op) {
            let local = bp.local(2);
            push!(sp, local);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldloc3 =>
        fn (pc, pb, op) {
            let local = bp.local(3);
            push!(sp, local);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldloc =>
        fn (pc, pb, op) {
            let local = bp.local(op.index as usize);
            push!(sp, local);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_stloc0 =>
        fn (pc, pb, op) {
            let val = pop!(sp);
            bp.set_local(0, val);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_stloc1 =>
        fn (pc, pb, op) {
            let val = pop!(sp);
            bp.set_local(1, val);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_stloc2 =>
        fn (pc, pb, op) {
            let val = pop!(sp);
            bp.set_local(2, val);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_stloc3 =>
        fn (pc, pb, op) {
            let val = pop!(sp);
            bp.set_local(3, val);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_stloc =>
        fn (pc, pb, op) {
            let val = pop!(sp);
            bp.set_local(op.index as usize, val);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldfree =>
        fn (pc, pb, op) {
            let callee = unsafe { bp.callee().downcast_unchecked::<Closure2>() };
            let val = callee[op.index as usize].get();
            push!(sp, val);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_stfree =>
        fn (pc, pb, op) {
            let val = pop!(sp);
            let callee = unsafe { bp.callee().downcast_unchecked::<Closure2>() };
            Gc::write(*ctx, callee)[op.index as usize].unlock().set(val);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_ldc_i4 =>
        fn (pc, pb, op) {
            let value = Value::new(op.value);
            push!(sp, value);
            become dispatch(pc, pb, ctx, bp, sp);
        },

        op_return_call =>
        fn (pc, pb, op) {
            let callee = pop!(sp);
            let argc = op.argc as usize;
            let callee_closure = callee.downcast::<Closure>();
            unsafe {
                let cb = callee_closure.meta.get().downcast::<CodeBlock>();
                let start = bp.start();
                let new_sp = start.sub(argc);

                std::ptr::copy(sp, new_sp, argc);
                sp = new_sp;

                push!(@slot sp, Slot { u8: argc as _ }); // FrameSlot::ArgumentCount
                push!(sp, callee); // FrameSlot::Callee
                push!(@slot sp, Slot { cb }); // FrameSlot::CodeBlock

                bp = (sp as *mut ActivationFrame<'gc>).as_mut_unchecked();

                become dispatch(0, cb, ctx, bp, sp);
            }
        },


        op_return_call_subr =>
        fn(pc, pb, op) {
            let _ = (pc, pb, op);
            panic!("op_return_call_subr is not implemented yet");
        },

        op_arity_exact =>
        fn (pc, pb, op) {
            become dispatch(pc, pb, ctx, bp, sp);
        }
    }
}

pub mod frame;
pub mod stack;
