//! C API for Capy runtime.
//!
//! Exports most of the runtime functionality to C-compatible interface.

use crate::runtime::{value::conversions::*, vm::load::load_thunk_in_vicinity};

use libc::c_char;
use std::{
    ffi::{CStr, c_void},
    marker::PhantomData,
    sync::Arc,
};

use crate::{
    prelude::*,
    runtime::{Context, Scheme, vm::threading::ThreadObject},
};

/// Safely convert a C string pointer to a Rust `&str`.
///
/// Returns `None` if the pointer is null or the string is not valid UTF-8.
///
/// # Safety
///
/// The caller must ensure `ptr` (if non-null) points to a valid, null-terminated
/// C string whose memory remains valid for the lifetime of the returned `&str`.
unsafe fn safe_cstr<'a>(ptr: *const c_char) -> Option<&'a str> {
    if ptr.is_null() {
        return None;
    }
    // SAFETY: Caller guarantees ptr is non-null and points to a valid C string.
    unsafe { CStr::from_ptr(ptr) }.to_str().ok()
}

pub struct Scm {
    scheme: Scheme,
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ScmRef(pub *mut Scm);

impl std::ops::Deref for ScmRef {
    type Target = Scm;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref().unwrap() }
    }
}

impl std::ops::DerefMut for ScmRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut().unwrap() }
    }
}
#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct ContextRef<'gc>(pub *const c_void, PhantomData<Context<'gc>>);

impl<'gc> ContextRef<'gc> {
    pub fn ctx(self) -> Context<'gc> {
        unsafe { Context::from_ptr(self.0 as *const ()) }
    }
}
/// Create a new Scheme thread instance.
///
/// The thread instance will be attached to currently active VM or
/// will initialize a new VM if there isn't any.
///
/// For thread creation look at [`scm_fork()`](scm_fork).
#[unsafe(no_mangle)]
pub extern "C" fn scm_new() -> ScmRef {
    let scm = Scheme::new();
    let capy = Scm { scheme: scm };
    ScmRef(Box::into_raw(Box::new(capy)))
}

/// Create a Scheme thread instance and VM from the given heap image.
///
/// This function can only be invoked once per process.
///
/// **Note:** This function is currently unimplemented and will panic if called.
#[unsafe(no_mangle)]
#[allow(dead_code)]
pub extern "C" fn scm_from_image(_image_data: *const u8, _image_size: usize) -> ScmRef {
    // Not yet implemented; return a null ScmRef so callers can detect failure.
    ScmRef(std::ptr::null_mut())
}

/// Returns the current accumulator value from the Scheme context state.
#[unsafe(no_mangle)]
pub fn scm_accumulator(ctx: ContextRef) -> Value {
    ctx.ctx().state().accumulator.get()
}

/// Sets the accumulator value in the Scheme context state.
#[unsafe(no_mangle)]
pub fn scm_set_accumulator<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>) {
    ctx.ctx().state().accumulator.set(value);
}

/// Free the Scheme thread instance created by [`scm_new()`](scm_new)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_free(scm: ScmRef) {
    // SAFETY: `scm.0` was allocated by `Box::into_raw` in `scm_new`. The caller
    // must ensure this is only called once per ScmRef and that no aliases remain.
    unsafe {
        let _ = Box::from_raw(scm.0);
    }
}

pub type ThreadFn =
    unsafe extern "C" fn(parent: ScmRef, arg: *mut std::ffi::c_void) -> *mut std::ffi::c_void;

/// Fork a new Scheme thread from the given parent thread.
///
/// New thread is created with a copy of the parent's dynamic state.
///
/// Returns thread-object for the newly created thread.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_fork<'gc>(
    parent: ContextRef<'gc>,
    init: ThreadFn,
    arg: *mut std::ffi::c_void,
) -> Value<'gc> {
    let thread_object = ThreadObject::new(*parent.ctx(), None);
    let dynamic_state = parent.ctx().dynamic_state().bits();
    let thread_object_bits = thread_object.as_ptr() as u64;
    let thread_spawned = Arc::new(std::sync::Barrier::new(2));
    let thread_spawned_clone = thread_spawned.clone();
    let arg = arg as usize;
    std::thread::spawn(move || {
        let scm = Scheme::forked(thread_object_bits, dynamic_state);
        thread_spawned_clone.wait();
        let parent_ptr = ScmRef(Box::into_raw(Box::new(Scm { scheme: scm })));
        unsafe { init(parent_ptr, arg as _) };
        let _ = unsafe { Box::from_raw(parent_ptr.0) };
    });

    thread_object.into()
}

pub type ScmEnterFn<'gc> =
    unsafe extern "C" fn(ctx: ContextRef<'gc>, arg: *mut std::ffi::c_void) -> libc::c_int;

/// Enter the Scheme runtime with the given Scheme instance.
///
/// This function will set up the necessary runtime state and call the provided
/// `enter` function with a context reference and the provided argument.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_enter<'gc>(
    scm: ScmRef,
    enter: ScmEnterFn<'gc>,
    arg: *mut std::ffi::c_void,
) -> libc::c_int {
    let scheme = &scm.scheme;
    let res = scheme.enter(|ctx| unsafe {
        let ctxref = ContextRef(ctx.as_ptr() as _, PhantomData);
        enter(ctxref, arg)
    });

    res
}

/// Given a Scheme context, module name, and variable name,
/// return the public variable reference or the default value
///
/// Example:
///
/// ```c
/// int my_enter(ContextRef ctx, void* arg) {
///     Value var = scm_public_ref(ctx, "boot cli", "enter", VALUE_NULL);
///     return 0;
/// }
///
/// ScmRef scm = scm_new();
/// scm_enter(scm, my_enter, NULL);
/// ```
#[unsafe(no_mangle)]
pub extern "C" fn scm_public_ref<'gc>(
    ctx: ContextRef<'gc>,
    module_name: *const c_char,
    name: *const c_char,
    default_value: Value<'gc>,
) -> Value<'gc> {
    // SAFETY: Caller must pass valid, null-terminated C strings.
    let (module_name, name) = match unsafe { (safe_cstr(module_name), safe_cstr(name)) } {
        (Some(m), Some(n)) => (m, n),
        _ => return default_value,
    };

    ctx.ctx()
        .public_ref(module_name, name)
        .unwrap_or(default_value)
}

/// Given a Scheme context, module name, and variable name,
/// return the private variable reference or the default value
///
/// Example:
/// ```c
/// int my_enter(ContextRef ctx, void* arg) {
///     Value var = scm_private_ref(ctx, "boot cli", "internal-variable", VALUE_NULL);
///     return 0;
/// }
///
/// ScmRef scm = scm_new();
/// scm_enter(scm, my_enter, NULL);
/// ```
#[unsafe(no_mangle)]
pub extern "C" fn scm_private_ref<'gc>(
    ctx: ContextRef<'gc>,
    module_name: *const c_char,
    name: *const c_char,
    default_value: Value<'gc>,
) -> Value<'gc> {
    // SAFETY: Caller must pass valid, null-terminated C strings.
    let (module_name, name) = match unsafe { (safe_cstr(module_name), safe_cstr(name)) } {
        (Some(m), Some(n)) => (m, n),
        _ => return default_value,
    };

    ctx.ctx()
        .private_ref(module_name, name)
        .unwrap_or(default_value)
}

/// Intern a symbol with the given name in the Scheme context.
#[unsafe(no_mangle)]
pub extern "C" fn scm_intern_symbol<'gc>(ctx: ContextRef<'gc>, name: *const c_char) -> Value<'gc> {
    // SAFETY: Caller must pass a valid, null-terminated C string.
    let name = match unsafe { safe_cstr(name) } {
        Some(s) => s,
        None => return Value::new(false),
    };
    ctx.ctx().intern(name)
}

/// Create a new Scheme string with the given data in the Scheme context.
#[unsafe(no_mangle)]
pub extern "C" fn scm_string<'gc>(ctx: ContextRef<'gc>, data: *const c_char) -> Value<'gc> {
    // SAFETY: Caller must pass a valid, null-terminated C string.
    let data = match unsafe { safe_cstr(data) } {
        Some(s) => s,
        None => return Value::new(false),
    };
    ctx.ctx().str(data)
}

/// Create a Scheme number from the given native numeric type.
///
/// Generated functions: `scm_uint32`, `scm_uint64`, `scm_int64`.
macro_rules! define_scm_from_numeric {
    ($($fn_name:ident, $ty:ty);* $(;)?) => {
        $(
            #[unsafe(no_mangle)]
            pub extern "C" fn $fn_name<'gc>(ctx: ContextRef<'gc>, value: $ty) -> Value<'gc> {
                value.into_value(ctx.ctx())
            }
        )*
    };
}

define_scm_from_numeric! {
    scm_uint32, u32;
    scm_uint64, u64;
    scm_int64, i64;
}

/// Try to convert a Scheme value to the given native numeric type.
/// Writes the result into `res` and returns `true` on success, `false` on failure.
///
/// Generated functions: `scm_to_u8`, `scm_to_u16`, `scm_to_u32`, `scm_to_u64`,
/// `scm_to_i8`, `scm_to_i16`, `scm_to_i32`, `scm_to_i64`, `scm_to_f32`, `scm_to_f64`.
macro_rules! define_scm_to_numeric {
    ($($fn_name:ident, $ty:ty);* $(;)?) => {
        $(
            #[unsafe(no_mangle)]
            pub extern "C" fn $fn_name<'gc>(
                ctx: ContextRef<'gc>,
                value: Value<'gc>,
                res: &mut $ty,
            ) -> bool {
                match <$ty>::try_from_value(ctx.ctx(), value) {
                    Ok(v) => {
                        *res = v;
                        true
                    }
                    Err(_) => false,
                }
            }
        )*
    };
}

define_scm_to_numeric! {
    scm_to_u8,  u8;
    scm_to_u16, u16;
    scm_to_u32, u32;
    scm_to_u64, u64;
    scm_to_i8,  i8;
    scm_to_i16, i16;
    scm_to_i32, i32;
    scm_to_i64, i64;
    scm_to_f32, f32;
    scm_to_f64, f64;
}

/// Convert a Scheme real number to an `f64`. Writes the result into `res` and
/// returns `true` on success, or `false` if `value` is not a number.
#[unsafe(no_mangle)]
pub extern "C" fn scm_real_to_f64<'gc>(
    ctx: ContextRef<'gc>,
    value: Value<'gc>,
    res: &mut f64,
) -> bool {
    match value.number() {
        Some(n) => {
            *res = n.real_to_f64(ctx.ctx());
            true
        }
        None => false,
    }
}

/// Construct a new Scheme pair (cons cell) from the given car and cdr values.
#[unsafe(no_mangle)]
pub extern "C" fn scm_cons<'gc>(
    ctx: ContextRef<'gc>,
    car: Value<'gc>,
    cdr: Value<'gc>,
) -> Value<'gc> {
    Value::cons(ctx.ctx(), car, cdr)
}

/// Set the car (first element) of a Scheme pair.
#[unsafe(no_mangle)]
pub extern "C" fn scm_set_car<'gc>(ctx: ContextRef<'gc>, pair: Value<'gc>, car: Value<'gc>) {
    pair.set_car(ctx.ctx(), car);
}

/// Set the cdr (second element / rest) of a Scheme pair.
#[unsafe(no_mangle)]
pub extern "C" fn scm_set_cdr<'gc>(ctx: ContextRef<'gc>, pair: Value<'gc>, cdr: Value<'gc>) {
    pair.set_cdr(ctx.ctx(), cdr);
}

/// Set the element at `index` in a Scheme vector to `value`.
#[unsafe(no_mangle)]
pub extern "C" fn scm_vector_set<'gc>(
    ctx: ContextRef<'gc>,
    vector: Value<'gc>,
    index: usize,
    value: Value<'gc>,
) {
    let vector = vector.downcast::<Vector>();
    if index >= vector.len() {
        return;
    }
    let wvector = Gc::write(*ctx.ctx(), vector);
    wvector[index].unlock().set(value);
}

/// Set the element at `index` in a Scheme tuple to `value`.
#[unsafe(no_mangle)]
pub extern "C" fn scm_tuple_set<'gc>(
    ctx: ContextRef<'gc>,
    tuple: Value<'gc>,
    index: usize,
    value: Value<'gc>,
) {
    let t = tuple.downcast::<Tuple>();
    if index >= t.len() {
        return;
    }
    let wt = Gc::write(*ctx.ctx(), t);
    wt[index].unlock().set(value);
}

/// Set the character at `index` in a Scheme string to `ch` (as a Unicode code point).
/// If `ch` is not a valid Unicode scalar value, the replacement character U+FFFD is used.
#[unsafe(no_mangle)]
pub extern "C" fn scm_string_set<'gc>(
    ctx: ContextRef<'gc>,
    string: Value<'gc>,
    index: usize,
    ch: u32,
) {
    const REPLACEMENT_CHAR: char = '\u{FFFD}';
    let ch = std::char::from_u32(ch).unwrap_or(REPLACEMENT_CHAR);

    let s = string.downcast::<Str>();
    if index >= s.len() {
        return;
    }
    Str::set(s, *ctx.ctx(), index, ch);
}

/// Return the character at `index` in a Scheme string as a Unicode code point.
#[unsafe(no_mangle)]
pub extern "C" fn scm_string_ref<'gc>(
    _ctx: ContextRef<'gc>,
    string: Value<'gc>,
    index: usize,
) -> u32 {
    match string.downcast::<Str>().get(index) {
        Some(ch) => ch as u32,
        None => 0,
    }
}

pub type PrepareCallFn =
    for<'gc> extern "C" fn(ctx: ContextRef<'gc>, args: &mut Value<'gc>, data: *mut c_void);

pub type FinishCallFn = for<'gc> extern "C" fn(
    ctx: ContextRef<'gc>,
    success: bool,
    result: Value<'gc>,
    data: *mut c_void,
) -> libc::c_int;

/// Call a named Scheme procedure from a given module.
///
/// `prepare` is invoked to build the argument list, and `finish` is invoked
/// with the result (or error) once the call completes.
#[unsafe(no_mangle)]
pub extern "C" fn scm_call(
    scm: ScmRef,
    mod_name: *const c_char,
    func_name: *const c_char,
    prepare: PrepareCallFn,
    data1: *mut c_void,
    finish: FinishCallFn,
    data2: *mut c_void,
) -> libc::c_int {
    // SAFETY: Caller must pass valid, null-terminated C strings for mod_name
    // and func_name. The function pointers `prepare` and `finish` must be valid.
    unsafe {
        let mod_name = match safe_cstr(mod_name) {
            Some(s) => s,
            None => return -1,
        };
        let func_name = match safe_cstr(func_name) {
            Some(s) => s,
            None => return -1,
        };
        scm.scheme.call(
            mod_name,
            func_name,
            |ctx, args| {
                let mut ls = Value::null();

                let _ = prepare(ContextRef(ctx.as_ptr() as _, PhantomData), &mut ls, data1);
                while !ls.is_null() {
                    let next = ls.cdr();
                    args.push(ls.car());
                    ls = next;
                }
            },
            |ctx, result| {
                let (succ, val) = match result {
                    Ok(v) => (true, v),
                    Err(e) => (false, e),
                };

                finish(ContextRef(ctx.as_ptr() as _, PhantomData), succ, val, data2)
            },
        )
    }
}

/// Resume a call using the current accumulator value as the procedure.
///
/// `prepare` builds the argument list, and `finish` receives the result or error.
#[unsafe(no_mangle)]
pub extern "C" fn scm_resume_accumulator<'gc>(
    scm: ScmRef,
    prepare: PrepareCallFn,
    data1: *mut c_void,
    finish: FinishCallFn,
    data2: *mut c_void,
) -> libc::c_int {
    let x = scm.scheme.call_value(
        |ctx, real_args| {
            let acc = ctx.state().accumulator.get();
            let mut args = Value::null();

            let _ = prepare(ContextRef(ctx.as_ptr() as _, PhantomData), &mut args, data1);
            while !args.is_null() {
                let next = args.cdr();
                real_args.push(args.car());
                args = next;
            }
            acc
        },
        |ctx, res| {
            let ctx = ContextRef(ctx.as_ptr() as _, PhantomData);
            finish(
                ctx,
                res.is_ok(),
                match res {
                    Ok(val) => val,
                    Err(val) => val,
                },
                data2,
            )
        },
    );

    x
}

/// Load and evaluate a Scheme source file by path. Returns 0 on success, -1 on failure.
#[unsafe(no_mangle)]
pub extern "C" fn scm_load_file(scm: ScmRef, name: *const c_char) -> libc::c_int {
    // SAFETY: Caller must pass a valid, null-terminated C string.
    let name = match unsafe { safe_cstr(name) } {
        Some(s) => s,
        None => return -1,
    };

    let x = scm.scheme.call_value(
        |ctx, _| {
            load_thunk_in_vicinity::<true>(ctx, &name, None::<String>, true, None)
                .unwrap_or_else(|_| Value::void())
        },
        |_, res| matches!(res, Ok(_)),
    );

    if x { 0 } else { -1 }
}

/// Return the current program arguments as a Scheme list of strings.
#[unsafe(no_mangle)]
pub extern "C" fn scm_program_arguments<'gc>(ctx: ContextRef<'gc>) -> Value<'gc> {
    crate::runtime::vm::base::get_program_arguments_fluid(ctx.ctx())
}

/// Initialize program arguments from a C-style `argc`/`argv` pair.
///
/// # Safety
///
/// `argv` must point to an array of at least `argc` valid, non-null C strings.
#[unsafe(no_mangle)]
pub extern "C" fn scm_program_arguments_init<'gc>(
    ctx: ContextRef<'gc>,
    argc: usize,
    argv: *const *const c_char,
) {
    // SAFETY: Caller must pass a valid argv array of `argc` non-null,
    // null-terminated C string pointers.
    unsafe {
        let mut ls = Value::null();
        for i in (0..argc).rev() {
            let arg = match safe_cstr(*argv.add(i)) {
                Some(s) => s,
                None => continue, // skip invalid arguments
            };
            let str_value = ctx.ctx().str(arg);
            ls = Value::cons(ctx.ctx(), str_value, ls);
        }

        crate::runtime::vm::base::program_arguments_fluid(ctx.ctx()).set(ctx.ctx(), ls);
    }
}

/// Set the program arguments to the given Scheme list of strings.
#[unsafe(no_mangle)]
pub extern "C" fn scm_set_program_arguments<'gc>(ctx: ContextRef<'gc>, args: Value<'gc>) {
    crate::runtime::vm::base::program_arguments_fluid(ctx.ctx()).set(ctx.ctx(), args);
}
