//! Pending exception API (throw / check / occurred / clear / describe).

use super::env::{SCM_REF_NULL, ScmRef, SniEnv, env_mut, ref_value};
use crate::runtime::sni::exception;
use crate::runtime::value::Value;
use crate::runtime::vm::exceptions::make_assertion_violation;

/// Install a pending assertion violation for a type-confused argument.
pub(crate) fn set_type_error<'gc>(
    env: &mut SniEnv<'gc>,
    who: &str,
    expected: &str,
    got: Value<'gc>,
) {
    let message = format!("expected {expected}");
    let condition = make_assertion_violation(env.ctx, Some(who), &message, &[got]);
    exception::throw(condition);
}

/// Install a pending assertion violation for an out-of-bounds index.
pub(crate) fn set_bounds_error<'gc>(env: &mut SniEnv<'gc>, who: &str, index: usize, len: usize) {
    let message = format!("index {index} out of bounds (length {len})");
    let condition =
        make_assertion_violation(env.ctx, Some(who), &message, &[Value::new(index as i32)]);
    exception::throw(condition);
}

/// Install `obj` as the pending exception. Returns 0 on success.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_throw(env: *mut SniEnv<'static>, obj: ScmRef) -> libc::c_int {
    if env.is_null() || obj.is_null() {
        return -1;
    }
    let v = unsafe { ref_value::<'_>(obj) };
    exception::throw(v);
    0
}

/// True if an exception is pending.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_exception_check(_env: *mut SniEnv<'static>) -> bool {
    exception::check()
}

/// Return a new local ref to the pending exception without clearing it.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_exception_occurred(env: *mut SniEnv<'static>) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let Some(v) = exception::occurred() else {
        return SCM_REF_NULL;
    };
    let e = unsafe { env_mut(env) };
    e.make_local(v)
}

/// Clear the pending exception. Returns a local ref to it, or null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_exception_clear(env: *mut SniEnv<'static>) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let Some(v) = exception::clear() else {
        return SCM_REF_NULL;
    };
    let e = unsafe { env_mut(env) };
    e.make_local(v)
}

/// Print the pending exception to stderr.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_exception_describe(_env: *mut SniEnv<'static>) {
    exception::describe();
}
