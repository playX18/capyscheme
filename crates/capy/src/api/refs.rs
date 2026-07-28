//! Local / global reference management (push/pop local frames, global refs).

use super::env::{SCM_REF_NULL, ScmRef, SniEnv, env_mut, global_delete, global_new, ref_value};

/// Push a new local frame. Returns 0 on success, -1 on failure.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_push_local_frame(
    env: *mut SniEnv<'static>,
    capacity: i32,
) -> libc::c_int {
    if env.is_null() {
        return -1;
    }
    let e = unsafe { env_mut(env) };
    let mut frame = super::env::LocalFrameState::new();
    if capacity > 0 {
        frame.ensure_capacity(capacity as usize);
    }
    e.frames.push(frame);
    0
}

/// Pop the top local frame. If `result` is non-null, promote it into the
/// previous frame as a new local ref and return that; otherwise return null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_pop_local_frame(env: *mut SniEnv<'static>, result: ScmRef) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    if e.frames.len() <= 1 {
        // Keep the root frame; just clear promoted value path.
        if result.is_null() {
            return SCM_REF_NULL;
        }
        let v = unsafe { ref_value(result) };
        return e.make_local(v);
    }
    let value = if result.is_null() {
        None
    } else {
        Some(unsafe { ref_value::<'_>(result) })
    };
    e.frames.pop();
    match value {
        Some(v) => e.make_local(v),
        None => SCM_REF_NULL,
    }
}

/// Ensure the current frame can hold `capacity` more refs. Returns 0 / -1.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_ensure_local_capacity(
    env: *mut SniEnv<'static>,
    capacity: i32,
) -> libc::c_int {
    if env.is_null() || capacity < 0 {
        return -1;
    }
    let e = unsafe { env_mut(env) };
    e.current_frame().ensure_capacity(capacity as usize);
    0
}

/// Create a new local ref for `obj` (or null).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_new_local_ref(env: *mut SniEnv<'static>, obj: ScmRef) -> ScmRef {
    if env.is_null() || obj.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value(obj) };
    e.make_local(v)
}

/// Delete a local ref from the current frame (no-op if not found).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_delete_local_ref(env: *mut SniEnv<'static>, obj: ScmRef) {
    if env.is_null() || obj.is_null() {
        return;
    }
    let e = unsafe { env_mut(env) };
    let _ = e.current_frame().delete(obj);
}

/// Create a global ref for `obj`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_new_global_ref(env: *mut SniEnv<'static>, obj: ScmRef) -> ScmRef {
    if obj.is_null() {
        return SCM_REF_NULL;
    }
    let _ = env; // globals are process-wide; env kept for signature uniformity.
    let v = unsafe { ref_value::<'_>(obj) };
    global_new(v)
}

/// Delete a global ref.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_delete_global_ref(_env: *mut SniEnv<'static>, obj: ScmRef) {
    unsafe { global_delete(obj) }
}

/// True if two refs refer to the same Scheme object identity (eq?).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_is_same(_env: *mut SniEnv<'static>, a: ScmRef, b: ScmRef) -> bool {
    if a.is_null() && b.is_null() {
        return true;
    }
    if a.is_null() || b.is_null() {
        return false;
    }
    let va = unsafe { ref_value::<'_>(a) };
    let vb = unsafe { ref_value::<'_>(b) };
    va == vb
}

/// True if `obj` is the null reference handle (not Scheme `'()`).
#[unsafe(no_mangle)]
pub extern "C" fn sni_ref_is_null(obj: ScmRef) -> bool {
    obj.is_null()
}
