//! Shared helpers for thunk implementations.

use super::make_assertion_violation;
use crate::runtime::vm::thunk_raise;
use crate::runtime::{
    Context, REGISTER_ARG_COUNT,
    value::{Closure, Number, Str, Value},
};

#[derive(Clone, Copy)]
pub struct RegisterArgs<'gc> {
    pub arg0: Value<'gc>,
    pub arg1: Value<'gc>,
    pub arg2: Value<'gc>,
    pub arg3: Value<'gc>,
    pub overflow: *const Value<'gc>,
}

impl<'gc> RegisterArgs<'gc> {
    pub fn get(self, index: usize) -> Value<'gc> {
        match index {
            0 => self.arg0,
            1 => self.arg1,
            2 => self.arg2,
            3 => self.arg3,
            // SAFETY: Preconditions verified by the surrounding code
            _ => unsafe { *self.overflow.add(index - REGISTER_ARG_COUNT) },
        }
    }

    pub fn collect_from(self, argc: usize, from: usize) -> Vec<Value<'gc>> {
        (from..argc).map(|index| self.get(index)).collect()
    }

    pub fn collect_range(self, argc: usize, from: usize, count: usize) -> Vec<Value<'gc>> {
        let end = from.saturating_add(count).min(argc);
        (from..end).map(|index| self.get(index)).collect()
    }
}

pub fn collect_register_args<'gc>(
    argc: usize,
    arg0: Value<'gc>,
    arg1: Value<'gc>,
    arg2: Value<'gc>,
    arg3: Value<'gc>,
    overflow: *const Value<'gc>,
    from: usize,
) -> Vec<Value<'gc>> {
    RegisterArgs {
        arg0,
        arg1,
        arg2,
        arg3,
        overflow,
    }
    .collect_from(argc, from)
}

pub fn save_register_args<'gc>(
    ctx: Context<'gc>,
    argc: usize,
    arg0: Value<'gc>,
    arg1: Value<'gc>,
    arg2: Value<'gc>,
    arg3: Value<'gc>,
) {
    ctx.state().gc_save.save(argc, [arg0, arg1, arg2, arg3]);
}

#[cfg(target_arch = "x86_64")]
unsafe extern "C" {
    #[link_name = "llvm.returnaddress"]
    fn returnaddress(_: i32) -> *const u8;
}

/// Return address of the current stack frame (for diagnostics).
///
/// # Safety
///
/// The caller must be executing in a frame that provides LLVM's return-address
/// intrinsic for the current target architecture.
pub unsafe fn llvm_return_address() -> *const u8 {
    unsafe { returnaddress(0) }
}

pub fn wrong_number_of_args_impl<'gc>(
    ctx: Context<'gc>,
    subr: Value<'gc>,
    got: usize,
    expected: isize,
    rands: &[Value<'gc>],
) -> Value<'gc> {
    let is_cont = subr.is::<Closure>() && subr.downcast::<Closure>().is_continuation();
    let msg = if is_cont {
        // SAFETY: Return address slot is valid — set up by the native calling convention
        let ret = unsafe { super::helpers::llvm_return_address() };
        backtrace::resolve(ret as _, |sym| {
            log::trace!(
                "WRONG ARGUMENTS TO {subr} (meta: {meta}) {sym:?}: {rands:?}",
                meta = if subr.is::<Closure>() {
                    subr.downcast::<Closure>().meta.get()
                } else {
                    Value::new(false)
                }
            );
        });
        if expected < 0 {
            format!("expected at least {} values, got {}", -expected, got)
        } else {
            format!("expected {} value(s), got {}", expected, got)
        }
    } else {
        // SAFETY: Return address slot is valid — set up by the native calling convention
        let ret = unsafe { super::helpers::llvm_return_address() };
        backtrace::resolve(ret as _, |sym| {
            log::trace!(
                "WRONG ARGUMENTS TO {subr} (meta: {meta}) {sym:?}: {rands:?}",
                meta = if subr.is::<Closure>() {
                    subr.downcast::<Closure>().meta.get()
                } else {
                    Value::new(false)
                }
            );
        });
        crate::runtime::vm::debug::print_stacktraces_impl(ctx);
        if expected < 0 {
            format!(
                "procedure expected at least {} arguments, got {}",
                -expected, got
            )
        } else {
            format!("procedure expected {} arguments, got {}", expected, got)
        }
    };

    let meta = if subr.is::<Closure>() {
        subr.downcast::<Closure>().meta.get()
    } else {
        Value::new(false)
    };
    make_assertion_violation(
        ctx,
        Value::new(false),
        Str::new(ctx, &msg, true).into(),
        &[subr, meta],
    )
}

pub fn require_number<'gc>(
    ctx: Context<'gc>,
    op: Value<'gc>,
    message: &str,
    val: Value<'gc>,
) -> Result<Number<'gc>, Value<'gc>> {
    let Some(n) = val.number() else {
        return Err(make_assertion_violation(
            ctx,
            op,
            Str::new(ctx, message, true).into(),
            &[val],
        ));
    };
    Ok(n)
}

pub fn require_exact_integer<'gc>(
    ctx: Context<'gc>,
    op: Value<'gc>,
    val: Value<'gc>,
) -> Result<Number<'gc>, Value<'gc>> {
    let n = require_number(ctx, op, "not a number", val)?;
    if !n.is_exact_integer() {
        return Err(make_assertion_violation(
            ctx,
            op,
            Str::new(ctx, "not an exact integer", true).into(),
            &[val],
        ));
    }
    Ok(n)
}

pub fn binop_numbers<'gc, F>(
    ctx: Context<'gc>,
    op: Value<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
    not_number_msg: &str,
    f: F,
) -> Value<'gc>
where
    F: FnOnce(Context<'gc>, Number<'gc>, Number<'gc>) -> Value<'gc>,
{
    let a = match require_number(ctx, op, not_number_msg, a) {
        Ok(n) => n,
        Err(e) => thunk_raise(ctx, e),
    };
    let b = match require_number(ctx, op, not_number_msg, b) {
        Ok(n) => n,
        Err(e) => thunk_raise(ctx, e),
    };
    f(ctx, a, b)
}
