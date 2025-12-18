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

pub struct Scm {
    #[allow(dead_code)]
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
#[unsafe(no_mangle)]
pub extern "C" fn scm_from_image(image_data: *const u8, image_size: usize) -> ScmRef {
    /*let image_slice = unsafe { std::slice::from_raw_parts(image_data, image_size) };
    let scm = Scheme::from_image(image_slice);
    let capy = Scm { scheme: scm };
    ScmRef(Box::into_raw(Box::new(capy)))*/
    todo!()
}

#[unsafe(no_mangle)]
pub fn scm_accumulator(ctx: ContextRef) -> Value {
    ctx.ctx().state().accumulator.get()
}

#[unsafe(no_mangle)]
pub fn scm_set_accumulator<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>) {
    ctx.ctx().state().accumulator.set(value);
}

/// Free the Scheme thread instance created by [`scm_new()`](scm_new)
#[unsafe(no_mangle)]
pub unsafe extern "C" fn scm_free(scm: ScmRef) {
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
/// ScmRef scm = scm_new();
///
/// scm_enter(scm, [](ContextRef ctx, void*){
///     Value var = scm_public_ref(ctx, "boot cli", "enter", VALUE_NULL);
/// }, NULL);
///
/// ```
#[unsafe(no_mangle)]
pub extern "C" fn scm_public_ref<'gc>(
    ctx: ContextRef<'gc>,
    module_name: *const c_char,
    name: *const c_char,
    default_value: Value<'gc>,
) -> Value<'gc> {
    let module_name = unsafe { CStr::from_ptr(module_name).to_str().unwrap() };
    let name = unsafe { CStr::from_ptr(name).to_str().unwrap() };

    ctx.ctx()
        .public_ref(module_name, name)
        .unwrap_or(default_value)
}

/// Given a Scheme context, module name, and variable name,
/// return the private variable reference or the default value
///
/// Example:
/// ```c
/// ScmRef scm = scm_new();
/// scm_enter(scm, [](ContextRef ctx, void*){
///     Value var = scm_private_ref(ctx, "boot cli", "internal-variable", VALUE_NULL);
/// }, NULL);
/// ```
#[unsafe(no_mangle)]
pub extern "C" fn scm_private_ref<'gc>(
    ctx: ContextRef<'gc>,
    module_name: *const c_char,
    name: *const c_char,
    default_value: Value<'gc>,
) -> Value<'gc> {
    let module_name = unsafe { CStr::from_ptr(module_name).to_str().unwrap() };
    let name = unsafe { CStr::from_ptr(name).to_str().unwrap() };

    ctx.ctx()
        .private_ref(module_name, name)
        .unwrap_or(default_value)
}

/// Intern a symbol with the given name in the Scheme context.
#[unsafe(no_mangle)]
pub extern "C" fn scm_intern_symbol<'gc>(ctx: ContextRef<'gc>, name: *const c_char) -> Value<'gc> {
    let name = unsafe { CStr::from_ptr(name).to_str().unwrap() };
    ctx.ctx().intern(name)
}

/// Create a new Scheme string with the given data in the Scheme context.
#[unsafe(no_mangle)]
pub extern "C" fn scm_string<'gc>(ctx: ContextRef<'gc>, data: *const c_char) -> Value<'gc> {
    let data = unsafe { CStr::from_ptr(data).to_str().unwrap() };
    ctx.ctx().str(data)
}

/// Create Scheme number from a 32-bit unsigned integer.
#[unsafe(no_mangle)]
pub extern "C" fn scm_uint32<'gc>(ctx: ContextRef<'gc>, value: u32) -> Value<'gc> {
    value.into_value(ctx.ctx())
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_uint64<'gc>(ctx: ContextRef<'gc>, value: u64) -> Value<'gc> {
    value.into_value(ctx.ctx())
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_int64<'gc>(ctx: ContextRef<'gc>, value: i64) -> Value<'gc> {
    value.into_value(ctx.ctx())
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_to_u8<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>, res: &mut u8) -> bool {
    match u8::try_from_value(ctx.ctx(), value) {
        Ok(v) => {
            *res = v;
            true
        }
        Err(_) => false,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_to_u16<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>, res: &mut u16) -> bool {
    match u16::try_from_value(ctx.ctx(), value) {
        Ok(v) => {
            *res = v;
            true
        }
        Err(_) => false,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_to_u32<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>, res: &mut u32) -> bool {
    match u32::try_from_value(ctx.ctx(), value) {
        Ok(v) => {
            *res = v;
            true
        }
        Err(_) => false,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_to_u64<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>, res: &mut u64) -> bool {
    match u64::try_from_value(ctx.ctx(), value) {
        Ok(v) => {
            *res = v;
            true
        }
        Err(_) => false,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_to_i64<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>, res: &mut i64) -> bool {
    match i64::try_from_value(ctx.ctx(), value) {
        Ok(v) => {
            *res = v;
            true
        }
        Err(_) => false,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_to_f64<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>, res: &mut f64) -> bool {
    match f64::try_from_value(ctx.ctx(), value) {
        Ok(v) => {
            *res = v;
            true
        }
        Err(_) => false,
    }
}

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

#[unsafe(no_mangle)]
pub extern "C" fn scm_to_i32<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>, res: &mut i32) -> bool {
    match i32::try_from_value(ctx.ctx(), value) {
        Ok(v) => {
            *res = v;
            true
        }
        Err(_) => false,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_to_f32<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>, res: &mut f32) -> bool {
    match f32::try_from_value(ctx.ctx(), value) {
        Ok(v) => {
            *res = v;
            true
        }
        Err(_) => false,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_to_i16<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>, res: &mut i16) -> bool {
    match i16::try_from_value(ctx.ctx(), value) {
        Ok(v) => {
            *res = v;
            true
        }
        Err(_) => false,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_to_i8<'gc>(ctx: ContextRef<'gc>, value: Value<'gc>, res: &mut i8) -> bool {
    match i8::try_from_value(ctx.ctx(), value) {
        Ok(v) => {
            *res = v;
            true
        }
        Err(_) => false,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_cons<'gc>(
    ctx: ContextRef<'gc>,
    car: Value<'gc>,
    cdr: Value<'gc>,
) -> Value<'gc> {
    Value::cons(ctx.ctx(), car, cdr)
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_set_car<'gc>(ctx: ContextRef<'gc>, pair: Value<'gc>, car: Value<'gc>) {
    pair.set_car(ctx.ctx(), car);
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_set_cdr<'gc>(ctx: ContextRef<'gc>, pair: Value<'gc>, cdr: Value<'gc>) {
    pair.set_cdr(ctx.ctx(), cdr);
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_vector_set<'gc>(
    ctx: ContextRef<'gc>,
    vector: Value<'gc>,
    index: usize,
    value: Value<'gc>,
) {
    let vector = vector.downcast::<Vector>();
    let wvector = Gc::write(*ctx.ctx(), vector);
    wvector[index].unlock().set(value);
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_tuple_set<'gc>(
    ctx: ContextRef<'gc>,
    tuple: Value<'gc>,
    index: usize,
    value: Value<'gc>,
) {
    let t = tuple.downcast::<Tuple>();
    let wt = Gc::write(*ctx.ctx(), t);
    wt[index].unlock().set(value);
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_string_set<'gc>(
    ctx: ContextRef<'gc>,
    string: Value<'gc>,
    index: usize,
    ch: u32,
) {
    const REPLACEMENT_CHAR: char = '\u{FFFD}';
    let ch = std::char::from_u32(ch).unwrap_or(REPLACEMENT_CHAR);

    Str::set(string.downcast(), *ctx.ctx(), index, ch);
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_string_ref<'gc>(
    _ctx: ContextRef<'gc>,
    string: Value<'gc>,
    index: usize,
) -> u32 {
    string.downcast::<Str>().get(index).unwrap() as u32
}

pub type PrepareCallFn =
    for<'gc> extern "C" fn(ctx: ContextRef<'gc>, args: &mut Value<'gc>, data: *mut c_void);

pub type FinishCallFn = for<'gc> extern "C" fn(
    ctx: ContextRef<'gc>,
    success: bool,
    result: Value<'gc>,
    data: *mut c_void,
) -> libc::c_int;

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
    unsafe {
        let mod_name = CStr::from_ptr(mod_name).to_str().unwrap();
        let func_name = CStr::from_ptr(func_name).to_str().unwrap();
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

#[unsafe(no_mangle)]
pub extern "C" fn scm_load_file(scm: ScmRef, name: *const c_char) -> libc::c_int {
    let name = unsafe { CStr::from_ptr(name as _).to_str().unwrap() };

    let x = scm.scheme.call_value(
        |ctx, _| load_thunk_in_vicinity::<true>(ctx, &name, None::<String>, true, None).unwrap(),
        |_, res| matches!(res, Ok(_)),
    );

    if x { 0 } else { -1 }
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_program_arguments<'gc>(ctx: ContextRef<'gc>) -> Value<'gc> {
    crate::runtime::vm::base::get_program_arguments_fluid(ctx.ctx())
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_program_arguments_init<'gc>(
    ctx: ContextRef<'gc>,
    argc: usize,
    argv: *const *const c_char,
) {
    unsafe {
        let mut ls = Value::null();
        for i in (0..argc).rev() {
            let arg = CStr::from_ptr(*argv.add(i)).to_str().unwrap();
            let str_value = ctx.ctx().str(arg);
            ls = Value::cons(ctx.ctx(), str_value, ls);
        }

        crate::runtime::vm::base::program_arguments_fluid(ctx.ctx()).set(ctx.ctx(), ls);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn scm_set_program_arguments<'gc>(ctx: ContextRef<'gc>, args: Value<'gc>) {
    crate::runtime::vm::base::program_arguments_fluid(ctx.ctx()).set(ctx.ctx(), args);
}
