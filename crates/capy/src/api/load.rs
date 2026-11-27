//! Load / eval / program-arguments helpers.

use libc::c_char;

use crate::runtime::{objectmodel::Value, vm::load::load_thunk_in_vicinity};

use super::env::{SCM_REF_NULL, ScmPtr, ScmRef, SniEnv, env_mut, ref_value, safe_cstr};

/// Load and evaluate a Scheme source file. Returns 0 on success, -1 on failure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_load_file(scm: ScmPtr, name: *const c_char) -> libc::c_int {
    if scm.0.is_null() {
        return -1;
    }
    let Some(name) = (unsafe { safe_cstr(name) }) else {
        return -1;
    };
    let ok = scm.scheme().call_value(
        |ctx, _| {
            load_thunk_in_vicinity::<true>(ctx, &name, None::<String>, true, None)
                .unwrap_or_else(|_| Value::void())
        },
        |_, res| res.is_ok(),
    );
    if ok { 0 } else { -1 }
}

/// Evaluates the Scheme text `source` the way `capy -c` does: every datum in turn, in the current
/// namespace. Returns a new local ref to the value of the last one (unspecified for none).
///
/// On an error (a syntax error in the text, or an exception from the code) it sets the pending
/// exception and returns `SCM_REF_NULL`; it never returns the text itself.
///
/// # Safety
/// `env` must be live and `source` a NUL-terminated string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_eval_string(
    env: *mut SniEnv<'static>,
    source: *const c_char,
) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let Some(src) = (unsafe { safe_cstr(source) }) else {
        return SCM_REF_NULL;
    };
    let e = unsafe { env_mut(env) };
    let text = e.ctx.str(src);
    let result = super::util::call_library(e, "boot cli", "eval-string/value", [text]);
    if result.is_null() && !crate::runtime::sni::exception::check() {
        let condition = crate::runtime::vm::exceptions::make_assertion_violation(
            e.ctx,
            Some("sni_eval_string"),
            "the evaluator (boot cli) is not available",
            &[],
        );
        crate::runtime::sni::exception::throw(condition);
    }
    result
}

/// Return program arguments as a Scheme list (new local ref).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_program_arguments(env: *mut SniEnv<'static>) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let args = crate::runtime::vm::base::get_program_arguments_fluid(e.ctx);
    e.make_local(args)
}

/// Initialize program arguments from argc/argv.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_program_arguments_init(
    env: *mut SniEnv<'static>,
    argc: i32,
    argv: *const *const c_char,
) {
    if env.is_null() || argc < 0 || argv.is_null() {
        return;
    }
    let e = unsafe { env_mut(env) };
    let mut ls = Value::null();
    for i in (0..argc as usize).rev() {
        let ptr = unsafe { *argv.add(i) };
        if ptr.is_null() {
            continue;
        }
        // Scheme strings are text: bad bytes become U+FFFD instead of dropping the argument.
        let arg = unsafe { std::ffi::CStr::from_ptr(ptr) }.to_string_lossy();
        let str_value = e.ctx.str(&arg);
        ls = Value::cons(e.ctx, str_value, ls);
    }
    crate::runtime::vm::base::program_arguments_fluid(e.ctx).set(e.ctx, ls);
}

/// Set program arguments from a Scheme list ref.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_set_program_arguments(env: *mut SniEnv<'static>, args: ScmRef) {
    if env.is_null() || args.is_null() {
        return;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value(args) };
    crate::runtime::vm::base::program_arguments_fluid(e.ctx).set(e.ctx, v);
}

/// Compatibility aliases used by older embeds / CLI.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_load_file(scm: ScmPtr, name: *const c_char) -> libc::c_int {
    unsafe { sni_load_file(scm, name) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_program_arguments_init(
    env: *mut SniEnv<'static>,
    argc: usize,
    argv: *const *const c_char,
) {
    unsafe { sni_program_arguments_init(env, argc as i32, argv) }
}
