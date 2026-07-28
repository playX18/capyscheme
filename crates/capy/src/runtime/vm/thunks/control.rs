use super::helpers::*;
use super::{ThunkResult, make_assertion_violation};
use crate::prelude::PROCEDURES;
use crate::runtime::class::{GenericDescriptor, GenericDispatchError};
use crate::runtime::vm::exceptions::{make_raise_condition, make_raise_condition_with_source};
use crate::runtime::vm::{default_exception_handler, default_retk as vm_default_retk};
use crate::runtime::{
    Context,
    value::{Closure, ReturnCode, Str, Value, Vector},
};

#[allow(clippy::not_unsafe_ptr_arg_deref)] // ABI entrypoint validates the register spill pointer locally.
pub fn wrong_number_of_args<'gc>(
    ctx: Context<'gc>,
    subr: Value<'gc>,
    got: usize,
    expected: isize,
    rands: *const Value<'gc>,
) -> Value<'gc> {
    //print_stacktraces_impl(ctx);
    // SAFETY: Pointer is valid for the given element count
    let rands = unsafe { std::slice::from_raw_parts(rands, got) };
    wrong_number_of_args_impl(ctx, subr, got, expected, rands)
}

#[allow(clippy::too_many_arguments)] // Fixed native-call ABI register arguments.
pub fn cons_rest_regs<'gc>(
    ctx: Context<'gc>,
    argc: usize,
    arg0: Value<'gc>,
    arg1: Value<'gc>,
    arg2: Value<'gc>,
    arg3: Value<'gc>,
    overflow: *const Value<'gc>,
    from: usize,
) -> Value<'gc> {
    let args = RegisterArgs {
        arg0,
        arg1,
        arg2,
        arg3,
        overflow,
    };
    let mut ls = Value::null();
    for index in (from..argc).rev() {
        let arg = args.get(index);
        ls = Value::cons(ctx, arg, ls);
    }

    ls
}

#[allow(clippy::too_many_arguments)] // Fixed native-call ABI register arguments.
pub fn raise_condition_regs<'gc>(
    ctx: Context<'gc>,
    code: usize,
    argc: usize,
    arg0: Value<'gc>,
    arg1: Value<'gc>,
    arg2: Value<'gc>,
    arg3: Value<'gc>,
    overflow: *const Value<'gc>,
    from: usize,
) -> Value<'gc> {
    save_register_args(ctx, argc, arg0, arg1, arg2, arg3);
    let count = argc.saturating_sub(from);
    let values = RegisterArgs {
        arg0,
        arg1,
        arg2,
        arg3,
        overflow,
    }
    .collect_range(argc, from, count);
    make_raise_condition(ctx, code, &values)
}

#[allow(clippy::too_many_arguments)] // Fixed native-call ABI register arguments.
pub fn raise_condition_with_source_regs<'gc>(
    ctx: Context<'gc>,
    code: usize,
    argc: usize,
    arg0: Value<'gc>,
    arg1: Value<'gc>,
    arg2: Value<'gc>,
    arg3: Value<'gc>,
    overflow: *const Value<'gc>,
    from: usize,
) -> Value<'gc> {
    save_register_args(ctx, argc, arg0, arg1, arg2, arg3);
    let args = RegisterArgs {
        arg0,
        arg1,
        arg2,
        arg3,
        overflow,
    };
    let source_index = argc.saturating_sub(1);
    let source = if argc > from {
        args.get(source_index)
    } else {
        Value::new(false)
    };
    let count = source_index.saturating_sub(from);
    let values = args.collect_range(argc, from, count);
    make_raise_condition_with_source(ctx, code, &values, source)
}

pub fn non_applicable<'gc>(ctx: Context<'gc>, subr: Value<'gc>) -> Value<'gc> {
    log::trace!("call {subr}");
    crate::runtime::vm::debug::print_stacktraces_impl(ctx);
    // SAFETY: Return address slot is valid — set up by the native calling convention
    let ret = unsafe { crate::runtime::vm::thunks::helpers::llvm_return_address() };
    backtrace::resolve(ret as _, |sym| {
        match subr.class_id() {
            Some(class_id) => {
                log::trace!(
                    "NON-APPLICABLE {subr}, class-id: {} called here:",
                    class_id.bits()
                )
            }
            None => log::trace!("NON-APPLICABLE {subr}, class-id: <none> called here:"),
        }
        log::trace!("{sym:?}");
    });
    make_assertion_violation(
        ctx,
        Value::new(false),
        Str::new(ctx, "attempt to call non-procedure", true).into(),
        &[subr],
    )
}

#[allow(clippy::too_many_arguments)] // Fixed native-call ABI register arguments.
pub fn generic_apply_regs<'gc>(
    ctx: Context<'gc>,
    generic: Value<'gc>,
    argc: usize,
    arg0: Value<'gc>,
    arg1: Value<'gc>,
    arg2: Value<'gc>,
    arg3: Value<'gc>,
    overflow: *const Value<'gc>,
    has_retk: u8,
) -> ThunkResult<'gc> {
    save_register_args(ctx, argc, arg0, arg1, arg2, arg3);

    if !generic.is::<GenericDescriptor>() {
        return ThunkResult {
            code: ReturnCode::ReturnErr as usize,
            value: non_applicable(ctx, generic),
        };
    }

    let first_arg = usize::from(has_retk != 0);
    let retk = (has_retk != 0).then_some(arg0);
    let args = collect_register_args(argc, arg0, arg1, arg2, arg3, overflow, first_arg);
    let generic_descriptor = generic.downcast::<GenericDescriptor>();
    match GenericDescriptor::invocation(ctx, generic_descriptor, &args) {
        Ok(invocation) => {
            let body = invocation.body();
            if !body.is::<Closure>() {
                return ThunkResult {
                    code: ReturnCode::ReturnErr as usize,
                    value: non_applicable(ctx, body),
                };
            }

            let ret = ctx.return_call(body, invocation.args().iter().copied(), retk);
            ThunkResult {
                code: ret.code as usize,
                value: ret.value,
            }
        }
        Err(GenericDispatchError::Arity) => {
            let message = Str::new(ctx, "not enough dispatch arguments", true);
            ThunkResult {
                code: ReturnCode::ReturnErr as usize,
                value: make_assertion_violation(ctx, Value::new(false), message.into(), &[generic]),
            }
        }
        Err(GenericDispatchError::NoApplicableMethod) => {
            let message = Str::new(ctx, "no applicable method", true);
            ThunkResult {
                code: ReturnCode::ReturnErr as usize,
                value: make_assertion_violation(ctx, Value::new(false), message.into(), &[generic]),
            }
        }
    }
}

#[allow(clippy::too_many_arguments)] // Fixed native-call ABI register arguments.
pub fn push_dframe_regs<'gc>(
    ctx: Context<'gc>,
    src: Value<'gc>,
    rator: Value<'gc>,
    argc: usize,
    arg0: Value<'gc>,
    arg1: Value<'gc>,
    arg2: Value<'gc>,
    arg3: Value<'gc>,
    overflow: *const Value<'gc>,
) -> Value<'gc> {
    assert!(
        argc > 0,
        "push_dframe_regs called without a return continuation"
    );

    let args = RegisterArgs {
        arg0,
        arg1,
        arg2,
        arg3,
        overflow,
    };
    let retk = args.get(0);
    let args = (1..argc).rev().fold(Value::null(), |acc, index| {
        let arg = args.get(index);
        Value::cons(ctx, arg, acc)
    });

    let info = Vector::from_slice(ctx, &[src, rator, args]);
    let key = crate::runtime::vm::debug::sym_stacktrace_key(ctx);

    crate::runtime::vm::control::push_cframe(
        ctx,
        key.into(),
        info.into(),
        retk.downcast::<Closure>(),
    )
}

#[allow(clippy::too_many_arguments)] // Fixed native-call ABI register arguments.
pub fn push_debug_dframe_regs<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    argc: usize,
    arg0: Value<'gc>,
    arg1: Value<'gc>,
    arg2: Value<'gc>,
    arg3: Value<'gc>,
    overflow: *const Value<'gc>,
) -> Value<'gc> {
    if argc == 0 || !rator.is::<Closure>() || !arg0.is::<Closure>() {
        return arg0;
    }

    let args = RegisterArgs {
        arg0,
        arg1,
        arg2,
        arg3,
        overflow,
    };
    let rator_closure = rator.downcast::<Closure>();
    if rator_closure.is_continuation() {
        return arg0;
    }

    let call_args = (1..argc).rev().fold(Value::null(), |acc, index| {
        let arg = args.get(index);
        Value::cons(ctx, arg, acc)
    });

    let src = crate::runtime::vm::debug::stacktrace_source_for_closure(ctx, rator_closure);
    let info = Vector::from_slice(ctx, &[src, rator, call_args]);
    let key = crate::runtime::vm::debug::sym_stacktrace_key(ctx);

    crate::runtime::vm::control::push_cframe(
        ctx,
        key.into(),
        info.into(),
        arg0.downcast::<Closure>(),
    )
}

pub fn exception_handler<'gc>(ctx: Context<'gc>) -> Value<'gc> {
    let handler = ctx.exception_handler();
    if let Some(handler) = handler {
        handler
    } else {
        let procs = PROCEDURES.fetch(ctx);
        let clos =
            procs.register_static_closure(ctx, default_exception_handler as _, Value::new(false));
        clos.into()
    }
}

pub fn default_retk<'gc>(ctx: Context<'gc>) -> Value<'gc> {
    let procs = PROCEDURES.fetch(ctx);
    let clos = procs.register_static_cont_closure(ctx, vm_default_retk as _, Value::new(false));
    clos.into()
}
