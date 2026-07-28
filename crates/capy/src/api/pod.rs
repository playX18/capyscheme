//! POD record APIs — movable bytevector backing.

use libc::c_char;
use std::sync::Mutex;

use crate::prelude::*;
use crate::runtime::value::Tuple;

use super::env::{SCM_REF_NULL, ScmRef, SniEnv, env_mut, ref_value, safe_cstr};
use super::exceptions::set_type_error;

/// Allocates a zeroed movable bytevector of `len` bytes for POD layouts.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_pod_bytevector(env: *mut SniEnv<'static>, len: usize) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let bv = ByteVector::new::<false>(e.ctx, len, true);
    bv.fill(0);
    e.make_local(bv.into())
}

/// Pointer to bytevector contents. Valid only while BV is rooted and until next GC/nest.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_bytevector_data(env: *mut SniEnv<'static>, bv: ScmRef) -> *mut u8 {
    if env.is_null() || bv.is_null() {
        return std::ptr::null_mut();
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value::<'_>(bv) };
    let Some(b) = v.try_as::<ByteVector>() else {
        set_type_error(e, "sni_bytevector_data", "bytevector", v);
        return std::ptr::null_mut();
    };
    b.contents().to_mut_ptr()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_bytevector_len(env: *mut SniEnv<'static>, bv: ScmRef) -> usize {
    if env.is_null() || bv.is_null() {
        return 0;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value::<'_>(bv) };
    match v.try_as::<ByteVector>() {
        Some(b) => b.len(),
        None => {
            set_type_error(e, "sni_bytevector_len", "bytevector", v);
            0
        }
    }
}

/// POD type descriptor: tuple `('type:pod-record name size align uid-or-#f)`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_register_pod_type(
    env: *mut SniEnv<'static>,
    name: *const c_char,
    size: usize,
    align: usize,
    uid: *const c_char,
) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let Some(name) = (unsafe { safe_cstr(name) }) else {
        return SCM_REF_NULL;
    };
    let uid_v = if uid.is_null() {
        Value::new(false)
    } else if let Some(u) = unsafe { safe_cstr(uid) } {
        Symbol::from_str(e.ctx, u).into()
    } else {
        Value::new(false)
    };
    let tag = Symbol::from_str(e.ctx, "type:pod-record");
    let desc = Tuple::from_slice(
        e.ctx,
        &[
            tag.into(),
            Symbol::from_str(e.ctx, name).into(),
            Value::new(size as i32),
            Value::new(align as i32),
            uid_v,
        ],
    );
    // Track sizes for type-check without re-parsing (best-effort).
    if let Ok(mut map) = POD_SIZES.lock() {
        map.insert(name.to_string(), size);
    }
    e.make_local(desc.into())
}

static POD_SIZES: std::sync::LazyLock<Mutex<std::collections::HashMap<String, usize>>> =
    std::sync::LazyLock::new(|| Mutex::new(std::collections::HashMap::new()));

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_pod_type_check(
    env: *mut SniEnv<'static>,
    pod_type: ScmRef,
    bv: ScmRef,
) -> bool {
    if env.is_null() || pod_type.is_null() || bv.is_null() {
        return false;
    }
    let e = unsafe { env_mut(env) };
    let tv = unsafe { ref_value::<'_>(pod_type) };
    let bv_v = unsafe { ref_value::<'_>(bv) };
    let Some(b) = bv_v.try_as::<ByteVector>() else {
        set_type_error(e, "sni_pod_type_check", "bytevector", bv_v);
        return false;
    };
    let Some(t) = tv.try_as::<Tuple>() else {
        set_type_error(e, "sni_pod_type_check", "pod type descriptor", tv);
        return false;
    };
    if t.len() < 3 {
        set_type_error(e, "sni_pod_type_check", "pod type descriptor", tv);
        return false;
    }
    let size_v = t[2].get();
    if !size_v.is_int32() {
        set_type_error(e, "sni_pod_type_check", "pod type descriptor", tv);
        return false;
    }
    let size = size_v.as_int32() as usize;
    b.len() == size
}
