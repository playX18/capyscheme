//! Value constructors, predicates, and accessors (ScmRef in/out).

use libc::c_char;

use crate::heap::Gc;
use crate::prelude::*;
use crate::runtime::value::conversions::*;

use super::env::{SCM_REF_NULL, ScmRef, SniEnv, env_mut, ref_value, safe_cstr};
use super::exceptions::{set_bounds_error, set_type_error};

macro_rules! pred {
    ($name:ident, $body:expr) => {
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $name(env: *mut SniEnv<'static>, obj: ScmRef) -> bool {
            if env.is_null() || obj.is_null() {
                return false;
            }
            let v = unsafe { ref_value::<'_>(obj) };
            $body(v)
        }
    };
}

pred!(sni_is_null, |v: Value<'_>| v.is_null());
pred!(sni_is_eof, |v: Value<'_>| v.is_eof());
pred!(sni_is_unspecified, |v: Value<'_>| v.is_unspecified());
pred!(sni_is_bool, |v: Value<'_>| {
    v == Value::new(true) || v == Value::new(false)
});
pred!(sni_is_true, |v: Value<'_>| v != Value::new(false));
pred!(sni_is_fixnum, |v: Value<'_>| v.is_int32());
pred!(sni_is_flonum, |v: Value<'_>| v.is_flonum());
pred!(sni_is_char, |v: Value<'_>| v.is_char());
pred!(sni_is_pair, |v: Value<'_>| v.is_pair());
pred!(sni_is_string, |v: Value<'_>| v.is::<Str>());
pred!(sni_is_symbol, |v: Value<'_>| v.is::<Symbol>());
pred!(sni_is_vector, |v: Value<'_>| v.is::<Vector>());
pred!(sni_is_bytevector, |v: Value<'_>| v.is::<ByteVector>());
pred!(sni_is_procedure, |v: Value<'_>| v.is::<Closure>());

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_null(env: *mut SniEnv<'static>) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    unsafe { env_mut(env) }.make_local(Value::null())
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_eof(env: *mut SniEnv<'static>) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    unsafe { env_mut(env) }.make_local(Value::eof())
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_unspecified(env: *mut SniEnv<'static>) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    unsafe { env_mut(env) }.make_local(Value::unspecified())
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_bool(env: *mut SniEnv<'static>, value: bool) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    unsafe { env_mut(env) }.make_local(Value::new(value))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_fixnum(env: *mut SniEnv<'static>, value: i32) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    unsafe { env_mut(env) }.make_local(Value::new(value))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_flonum(env: *mut SniEnv<'static>, value: f64) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    e.make_local(Value::new(value))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_char(env: *mut SniEnv<'static>, ch: u32) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let c = char::from_u32(ch).unwrap_or('\u{FFFD}');
    e.make_local(Value::new(c))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_intern_symbol(
    env: *mut SniEnv<'static>,
    name: *const c_char,
) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let Some(name) = (unsafe { safe_cstr(name) }) else {
        return SCM_REF_NULL;
    };
    let e = unsafe { env_mut(env) };
    e.make_local(e.ctx.intern(name))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_string(env: *mut SniEnv<'static>, data: *const c_char) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let Some(data) = (unsafe { safe_cstr(data) }) else {
        return SCM_REF_NULL;
    };
    let e = unsafe { env_mut(env) };
    e.make_local(e.ctx.str(data))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_string_utf8(
    env: *mut SniEnv<'static>,
    data: *const u8,
    len: usize,
) -> ScmRef {
    if env.is_null() || data.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let bytes = unsafe { std::slice::from_raw_parts(data, len) };
    let text = String::from_utf8_lossy(bytes);
    e.make_local(e.ctx.str(&text))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_cons(env: *mut SniEnv<'static>, car: ScmRef, cdr: ScmRef) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let car_v = if car.is_null() {
        Value::null()
    } else {
        unsafe { ref_value(car) }
    };
    let cdr_v = if cdr.is_null() {
        Value::null()
    } else {
        unsafe { ref_value(cdr) }
    };
    e.make_local(Value::cons(e.ctx, car_v, cdr_v))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_car(env: *mut SniEnv<'static>, pair: ScmRef) -> ScmRef {
    if env.is_null() || pair.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value(pair) };
    if !v.is_pair() {
        set_type_error(e, "sni_car", "pair", v);
        return SCM_REF_NULL;
    }
    e.make_local(v.car())
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_cdr(env: *mut SniEnv<'static>, pair: ScmRef) -> ScmRef {
    if env.is_null() || pair.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value(pair) };
    if !v.is_pair() {
        set_type_error(e, "sni_cdr", "pair", v);
        return SCM_REF_NULL;
    }
    e.make_local(v.cdr())
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_set_car(env: *mut SniEnv<'static>, pair: ScmRef, car: ScmRef) {
    if env.is_null() || pair.is_null() {
        return;
    }
    let e = unsafe { env_mut(env) };
    let p = unsafe { ref_value(pair) };
    let c = if car.is_null() {
        Value::null()
    } else {
        unsafe { ref_value(car) }
    };
    if p.is_pair() {
        p.set_car(e.ctx, c);
    } else {
        set_type_error(e, "sni_set_car", "pair", p);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_set_cdr(env: *mut SniEnv<'static>, pair: ScmRef, cdr: ScmRef) {
    if env.is_null() || pair.is_null() {
        return;
    }
    let e = unsafe { env_mut(env) };
    let p = unsafe { ref_value(pair) };
    let c = if cdr.is_null() {
        Value::null()
    } else {
        unsafe { ref_value(cdr) }
    };
    if p.is_pair() {
        p.set_cdr(e.ctx, c);
    } else {
        set_type_error(e, "sni_set_cdr", "pair", p);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_vector(
    env: *mut SniEnv<'static>,
    len: usize,
    fill: ScmRef,
) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let fill_v = if fill.is_null() {
        Value::unspecified()
    } else {
        unsafe { ref_value(fill) }
    };
    let vec = Vector::new::<false>(e.ctx, len, fill_v);
    e.make_local(Value::from(vec))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_vector_length(env: *mut SniEnv<'static>, vector: ScmRef) -> usize {
    if env.is_null() || vector.is_null() {
        return 0;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value::<'_>(vector) };
    match v.try_as::<Vector>() {
        Some(vec) => vec.len(),
        None => {
            set_type_error(e, "sni_vector_length", "vector", v);
            0
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_vector_ref(
    env: *mut SniEnv<'static>,
    vector: ScmRef,
    index: usize,
) -> ScmRef {
    if env.is_null() || vector.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value(vector) };
    let Some(vec) = v.try_as::<Vector>() else {
        set_type_error(e, "sni_vector_ref", "vector", v);
        return SCM_REF_NULL;
    };
    if index >= vec.len() {
        set_bounds_error(e, "sni_vector_ref", index, vec.len());
        return SCM_REF_NULL;
    }
    e.make_local(vec[index].get())
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_vector_set(
    env: *mut SniEnv<'static>,
    vector: ScmRef,
    index: usize,
    value: ScmRef,
) {
    if env.is_null() || vector.is_null() {
        return;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value(vector) };
    let Some(vec) = v.try_as::<Vector>() else {
        set_type_error(e, "sni_vector_set", "vector", v);
        return;
    };
    if index >= vec.len() {
        set_bounds_error(e, "sni_vector_set", index, vec.len());
        return;
    }
    let val = if value.is_null() {
        Value::null()
    } else {
        unsafe { ref_value(value) }
    };
    let wvector = Gc::write(e.ctx, vec);
    wvector[index].unlock().set(val);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_make_bytevector(
    env: *mut SniEnv<'static>,
    len: usize,
    fill: u8,
) -> ScmRef {
    if env.is_null() {
        return SCM_REF_NULL;
    }
    let e = unsafe { env_mut(env) };
    let bv = ByteVector::new::<false>(e.ctx, len, true);
    bv.fill(fill);
    e.make_local(bv.into())
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_bytevector_length(env: *mut SniEnv<'static>, bv: ScmRef) -> usize {
    if env.is_null() || bv.is_null() {
        return 0;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value::<'_>(bv) };
    match v.try_as::<ByteVector>() {
        Some(b) => b.len(),
        None => {
            set_type_error(e, "sni_bytevector_length", "bytevector", v);
            0
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_bytevector_ref(
    env: *mut SniEnv<'static>,
    bv: ScmRef,
    index: usize,
) -> i32 {
    if env.is_null() || bv.is_null() {
        return -1;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value::<'_>(bv) };
    let Some(b) = v.try_as::<ByteVector>() else {
        set_type_error(e, "sni_bytevector_ref", "bytevector", v);
        return -1;
    };
    let slice = b.as_slice();
    match slice.get(index) {
        Some(&byte) => byte as i32,
        None => {
            set_bounds_error(e, "sni_bytevector_ref", index, b.len());
            -1
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_bytevector_set(
    env: *mut SniEnv<'static>,
    bv: ScmRef,
    index: usize,
    byte: u8,
) {
    if env.is_null() || bv.is_null() {
        return;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value(bv) };
    let Some(b) = v.try_as::<ByteVector>() else {
        set_type_error(e, "sni_bytevector_set", "bytevector", v);
        return;
    };
    if index < b.len() {
        // SAFETY: mutable bytevector contents; index bounds-checked.
        unsafe {
            b.as_slice_mut_unchecked()[index] = byte;
        }
    } else {
        set_bounds_error(e, "sni_bytevector_set", index, b.len());
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_string_length(env: *mut SniEnv<'static>, s: ScmRef) -> usize {
    if env.is_null() || s.is_null() {
        return 0;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value::<'_>(s) };
    match v.try_as::<Str>() {
        Some(string) => string.len(),
        None => {
            set_type_error(e, "sni_string_length", "string", v);
            0
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_string_ref(env: *mut SniEnv<'static>, s: ScmRef, index: usize) -> u32 {
    if env.is_null() || s.is_null() {
        return 0;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value::<'_>(s) };
    let Some(string) = v.try_as::<Str>() else {
        set_type_error(e, "sni_string_ref", "string", v);
        return 0;
    };
    match string.get(index) {
        Some(ch) => ch as u32,
        None => {
            set_bounds_error(e, "sni_string_ref", index, string.len());
            0
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_string_set(
    env: *mut SniEnv<'static>,
    s: ScmRef,
    index: usize,
    ch: u32,
) {
    if env.is_null() || s.is_null() {
        return;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value(s) };
    let Some(string) = v.try_as::<Str>() else {
        set_type_error(e, "sni_string_set", "string", v);
        return;
    };
    let ch = char::from_u32(ch).unwrap_or('\u{FFFD}');
    if index < string.len() {
        Str::set(string, e.ctx, index, ch);
    } else {
        set_bounds_error(e, "sni_string_set", index, string.len());
    }
}

/// Copy Scheme string UTF-8 into `buf`. Returns false if too small / not a string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_string_to_utf8(
    env: *mut SniEnv<'static>,
    value: ScmRef,
    buf: *mut c_char,
    capacity: usize,
    written: *mut usize,
) -> bool {
    if env.is_null() || value.is_null() {
        return false;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value::<'_>(value) };
    let Some(string) = v.try_as::<Str>() else {
        set_type_error(e, "sni_string_to_utf8", "string", v);
        if !written.is_null() {
            unsafe { *written = 0 };
        }
        return false;
    };
    let text = string.to_string();
    let bytes = text.as_bytes();
    if !written.is_null() {
        unsafe { *written = bytes.len() };
    }
    if buf.is_null() || capacity <= bytes.len() {
        return false;
    }
    unsafe {
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), buf.cast::<u8>(), bytes.len());
        *buf.add(bytes.len()) = 0;
    }
    true
}

/// Print any value to UTF-8 buffer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_value_to_utf8(
    _env: *mut SniEnv<'static>,
    value: ScmRef,
    buf: *mut c_char,
    capacity: usize,
    written: *mut usize,
) -> bool {
    if value.is_null() {
        return false;
    }
    let v = unsafe { ref_value::<'_>(value) };
    let text = v.to_string();
    let bytes = text.as_bytes();
    if !written.is_null() {
        unsafe { *written = bytes.len() };
    }
    if buf.is_null() || capacity <= bytes.len() {
        return false;
    }
    unsafe {
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), buf.cast::<u8>(), bytes.len());
        *buf.add(bytes.len()) = 0;
    }
    true
}

macro_rules! define_scm_from_numeric {
    ($($fn_name:ident, $ty:ty);* $(;)?) => {
        $(
            #[unsafe(no_mangle)]
            pub unsafe extern "C" fn $fn_name(env: *mut SniEnv<'static>, value: $ty) -> ScmRef {
                if env.is_null() {
                    return SCM_REF_NULL;
                }
                let e = unsafe { env_mut(env) };
                e.make_local(value.into_value(e.ctx))
            }
        )*
    };
}

define_scm_from_numeric! {
    sni_uint32, u32;
    sni_uint64, u64;
    sni_int64, i64;
}

macro_rules! define_scm_to_numeric {
    ($($fn_name:ident, $ty:ty);* $(;)?) => {
        $(
            #[unsafe(no_mangle)]
            pub unsafe extern "C" fn $fn_name(
                env: *mut SniEnv<'static>,
                value: ScmRef,
                res: *mut $ty,
            ) -> bool {
                if env.is_null() || value.is_null() || res.is_null() {
                    return false;
                }
                let e = unsafe { env_mut(env) };
                let v = unsafe { ref_value(value) };
                match <$ty>::try_from_value(e.ctx, v) {
                    Ok(n) => {
                        unsafe { *res = n };
                        true
                    }
                    Err(_) => false,
                }
            }
        )*
    };
}

define_scm_to_numeric! {
    sni_to_u8,  u8;
    sni_to_u16, u16;
    sni_to_u32, u32;
    sni_to_u64, u64;
    sni_to_i8,  i8;
    sni_to_i16, i16;
    sni_to_i32, i32;
    sni_to_i64, i64;
    sni_to_f32, f32;
    sni_to_f64, f64;
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_real_to_f64(
    env: *mut SniEnv<'static>,
    value: ScmRef,
    res: *mut f64,
) -> bool {
    if env.is_null() || value.is_null() || res.is_null() {
        return false;
    }
    let e = unsafe { env_mut(env) };
    let v = unsafe { ref_value(value) };
    match v.number() {
        Some(n) => {
            unsafe { *res = n.real_to_f64(e.ctx) };
            true
        }
        None => false,
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn sni_to_bool(_env: *mut SniEnv<'static>, value: ScmRef) -> bool {
    if value.is_null() {
        return false;
    }
    let v = unsafe { ref_value::<'_>(value) };
    v != Value::new(false)
}
