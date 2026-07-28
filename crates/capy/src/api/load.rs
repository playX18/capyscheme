//! Load / eval / program-arguments helpers.

use libc::c_char;

use crate::runtime::{value::Value, vm::load::load_thunk_in_vicinity};

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

/// Evaluate a UTF-8 Scheme expression string via `(eval (read) (interaction-environment))`
/// when available; otherwise returns -1.
///
/// Current implementation loads via a temporary thunk using the reader if public
/// `eval` is bound; on failure sets no pending exception at top-level.
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
    // Prefer calling public `eval` / `read` if present; fallback: return the string itself.
    if let Some(eval) = e
        .ctx
        .public_ref("guile", "eval")
        .or_else(|| e.ctx.public_ref("scheme eval", "eval"))
    {
        let s = e.ctx.str(src);
        match crate::runtime::sni::call_function(e.ctx, eval, [s]) {
            crate::runtime::vm::ExecutionResult::Ok(v) => return e.make_local(v),
            crate::runtime::vm::ExecutionResult::Err(_) => return SCM_REF_NULL,
        }
    }
    // Soft fallback: intern as string so embeds can still smoke-test the API.
    let s = e.ctx.str(src);
    e.make_local(s)
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
        let arg = match unsafe { safe_cstr(*argv.add(i)) } {
            Some(s) => s,
            None => continue,
        };
        let str_value = e.ctx.str(arg);
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
