//! Shared helpers for thunk implementations.

use super::make_assertion_violation;
use crate::runtime::vm::thunk_raise;
use crate::runtime::{
    MutationContext,
    value::{Closure, Number, Str, Value},
};

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
    ctx: MutationContext<'gc>,
    subr: Value<'gc>,
    got: usize,
    expected: isize,
    rands: &[Value<'gc>],
) -> Value<'gc> {
    let is_cont = subr.is::<Closure>() && subr.downcast::<Closure>().is_continuation();
    let msg = if is_cont {
        log::trace!("WRONG ARGUMENTS TO {subr}: {rands:?}");
        if expected < 0 {
            format!("expected at least {} values, got {}", -expected, got)
        } else {
            format!("expected {} value(s), got {}", expected, got)
        }
    } else {
        log::trace!("WRONG ARGUMENTS TO {subr}: {rands:?}");
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
    ctx: MutationContext<'gc>,
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
    ctx: MutationContext<'gc>,
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
    ctx: MutationContext<'gc>,
    op: Value<'gc>,
    a: Value<'gc>,
    b: Value<'gc>,
    not_number_msg: &str,
    f: F,
) -> Value<'gc>
where
    F: FnOnce(MutationContext<'gc>, Number<'gc>, Number<'gc>) -> Value<'gc>,
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
