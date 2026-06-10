//! Core virtual machine: call dispatch, stack management, and Scheme ↔ native transitions.

use std::{
    io::ErrorKind,
    marker::PhantomData,
    ops::{FromResidual, Try},
    sync::atomic::Ordering,
};

use crate::rsgc::{Trace, object::builtin_class_ids};
use crate::runtime::{
    Context,
    value::{
        Closure, ConversionError, NativeReturn, PROCEDURES, ReturnCode, Str, Symbol, TryIntoValues,
        Value, Vector,
    },
    vm::{io::IoOperation, trampolines::get_trampoline_into_scheme},
};

pub mod arith;
pub mod base;
pub mod continuation;
pub mod control;
pub mod debug;
pub mod dl;
pub mod errors;
pub mod eval;
pub mod exceptions;
pub mod expand;
pub mod extensions;
pub mod ffi;
pub mod gc;
pub mod hash;
pub mod interrupts;
pub mod io;
pub mod libraries;
pub mod list;
pub mod load;
pub mod memoize;
pub mod records;
pub mod strings;
pub mod syntax;
pub mod threading;
pub mod throw;
pub mod thunks;
pub mod trampolines;
pub mod vector;

fn is_procedure(value: Value<'_>) -> bool {
    value
        .class_id()
        .is_some_and(|id| matches!(id.bits(), builtin_class_ids::CLOSURE_PROC))
}

fn is_continuation(value: Value<'_>) -> bool {
    value
        .class_id()
        .is_some_and(|id| id.bits() == builtin_class_ids::CLOSURE_K)
}

/// Perform call into the Scheme code in `rator` with `args`.
///
/// This function performs *nested* call into Scheme. For calling Scheme
/// from native code use [`return_call`](NativeCallContext::return_call) method.
///
/// Once this function is called, current thread nest level is increased by 1
/// and if it is greater than 1, GC safepoints and interrupts will be ignored until
/// nested level is back to 1.
pub fn call_scheme<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> VMResult<'gc> {
    let procs = PROCEDURES.fetch(*ctx);

    let retk = procs.register_static_cont_closure(ctx, default_retk, Value::null());

    call_scheme_with_k(ctx, retk.into(), rator, args)
}

/// Call Scheme code with explicit return continuation.
pub extern "C" fn call_scheme_with_k<'gc>(
    ctx: Context<'gc>,
    retk: Value<'gc>,
    rator: Value<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> VMResult<'gc> {
    if !rator.is::<Closure>() {
        return VMResult::Err(rator);
    }
    let guard = NestedSchemeCallGuard::new(ctx);

    let (argc, regs) = ctx.prepare_scheme_call_args(args, Some(retk));

    let f = get_trampoline_into_scheme().to_ptr::<()>();

    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let f: extern "C-unwind" fn(
            Context<'gc>,
            Value<'gc>,
            usize,
            Value<'gc>,
            Value<'gc>,
            Value<'gc>,
            Value<'gc>,
        ) -> NativeReturn<'gc> = std::mem::transmute(f);

        let val = trampoline(ctx, rator, argc, regs[0], regs[1], regs[2], regs[3], f);
        drop(guard);

        match val.code {
            ReturnCode::Continue => unreachable!("cannot continue into native code"),
            ReturnCode::ReturnErr => VMResult::Err(val.value),
            ReturnCode::ReturnOk => VMResult::Ok(val.value),
        }
    }
}

/// Continue execution by invoking a Scheme continuation.
///
/// # Safety
///
/// `ctx` must be an active runtime context, `cont` must be a live continuation
/// value for that context, and `args` must contain values valid in `ctx`.
// SAFETY: Invariants are upheld at this call site
pub unsafe extern "C" fn continue_to<'gc>(
    ctx: Context<'gc>,
    cont: Value<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> VMResult<'gc> {
    if !cont.is::<Closure>() {
        return VMResult::Err(cont);
    }
    let guard = NestedSchemeCallGuard::new(ctx);

    let (argc, regs) = ctx.prepare_scheme_call_args(args, None);

    let f = get_trampoline_into_scheme().to_ptr::<()>();

    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let f: extern "C-unwind" fn(
            Context<'gc>,
            Value<'gc>,
            usize,
            Value<'gc>,
            Value<'gc>,
            Value<'gc>,
            Value<'gc>,
        ) -> NativeReturn<'gc> = std::mem::transmute(f);

        let val = trampoline(ctx, cont, argc, regs[0], regs[1], regs[2], regs[3], f);
        drop(guard);
        match val.code {
            ReturnCode::Continue => unreachable!("cannot continue into native code"),
            ReturnCode::ReturnErr => VMResult::Err(val.value),
            ReturnCode::ReturnOk => VMResult::Ok(val.value),
        }
    }
}

struct NestedSchemeCallGuard<'gc> {
    ctx: Context<'gc>,
    old_runstack: mmtk::util::Address,
    old_nest_level: usize,
}

impl<'gc> NestedSchemeCallGuard<'gc> {
    fn new(ctx: Context<'gc>) -> Self {
        let old_runstack = ctx.state().runstack.get();
        let old_nest_level = ctx.state().nest_level.fetch_add(1, Ordering::Relaxed);
        Self {
            ctx,
            old_runstack,
            old_nest_level,
        }
    }
}

impl Drop for NestedSchemeCallGuard<'_> {
    fn drop(&mut self) {
        self.ctx
            .state()
            .nest_level
            .store(self.old_nest_level, Ordering::Relaxed);
        self.ctx.state().runstack.set(self.old_runstack);
    }
}

#[inline(never)]
extern "C" fn trampoline<'a>(
    ctx: Context<'a>,
    rator: Value<'a>,
    argc: usize,
    arg0: Value<'a>,
    arg1: Value<'a>,
    arg2: Value<'a>,
    arg3: Value<'a>,
    f: extern "C-unwind" fn(
        Context<'a>,
        Value<'a>,
        usize,
        Value<'a>,
        Value<'a>,
        Value<'a>,
        Value<'a>,
    ) -> NativeReturn<'a>,
) -> NativeReturn<'a> {
    f(ctx, rator, argc, arg0, arg1, arg2, arg3)
}

pub(crate) extern "C-unwind" fn default_retk<'gc>(
    ctx: Context<'gc>,
    _rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
) -> NativeReturn<'gc> {
    let value = if num_rands == 0 {
        Value::undefined()
    } else if num_rands == 1 {
        // SAFETY: Pointer is valid for the given element count
        unsafe { *rands }
    } else {
        // SAFETY: Pointer is valid for the given element count
        let args = unsafe { std::slice::from_raw_parts(rands, num_rands) };
        Vector::from_slice(*ctx, args).into()
    };

    NativeReturn {
        code: ReturnCode::ReturnOk,
        value,
    }
}

pub(crate) extern "C-unwind" fn default_exception_handler<'gc>(
    _ctx: Context<'gc>,
    _rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
    _retk: Value<'gc>,
) -> NativeReturn<'gc> {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        assert!(num_rands >= 1, "exception handler called with no arguments");
        let exception = *rands;
        NativeReturn {
            code: ReturnCode::ReturnErr,
            value: exception,
        }
    }
}

/// A VM execution result.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Trace)]
#[collect(no_drop)]
#[repr(C)]
pub enum VMResult<'gc> {
    /// VM executed succsefully and value was returned.
    Ok(Value<'gc>),
    /// VM executed, but error was thrown.
    Err(Value<'gc>),
}

impl<'gc> FromResidual<Value<'gc>> for VMResult<'gc> {
    fn from_residual(residual: Value<'gc>) -> Self {
        Self::Err(residual)
    }
}

impl<'gc> std::ops::Residual<Value<'gc>> for Value<'gc> {
    type TryType = VMResult<'gc>;
}

impl<'gc> Try for VMResult<'gc> {
    type Output = Value<'gc>;
    type Residual = Value<'gc>;

    fn from_output(output: Self::Output) -> Self {
        Self::Ok(output)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            VMResult::Ok(v) => std::ops::ControlFlow::Continue(v),
            VMResult::Err(e) => std::ops::ControlFlow::Break(e),
        }
    }
}

pub struct NativeCallContext<'a, 'gc, R: TryIntoValues<'gc> = Value<'gc>> {
    pub ctx: Context<'gc>,
    rator: Value<'gc>,
    rands: &'a [Value<'gc>],

    pub(crate) retk: Value<'gc>,

    return_data: PhantomData<R>,
}

impl<'a, 'gc, R: TryIntoValues<'gc>> NativeCallContext<'a, 'gc, R> {
    /// Construct native call context from raw parts.
    ///
    /// # Safety
    ///
    /// Rands must be a valid pointer created by runtime, not arbitrary pointer.
    // SAFETY: Caller must ensure preconditions are met (see fn docs)
    pub unsafe fn from_raw(
        ctx: Context<'gc>,
        rator: Value<'gc>,
        rands: *const Value<'gc>,
        num_rands: usize,
        retk: Value<'gc>,
    ) -> Self {
        Self {
            ctx,
            rator,
            // SAFETY: Pointer is valid for the given element count
            rands: unsafe { std::slice::from_raw_parts(rands, num_rands) },

            retk,
            return_data: PhantomData,
        }
    }

    pub fn rands(&self) -> &[Value<'gc>] {
        self.rands
    }

    pub fn rator(&self) -> Value<'gc> {
        self.rator
    }

    pub fn is_continuation(&self) -> bool {
        is_continuation(self.rator)
    }

    pub fn return_(self, values: R) -> NativeCallReturn<'gc> {
        let values = match values.try_into_values(self.ctx) {
            Ok(values) => values,
            Err(err) => {
                let handler = self.ctx.exception_handler().unwrap_or_else(|| {
                    PROCEDURES
                        .fetch(*self.ctx)
                        .register_static_closure(
                            self.ctx,
                            default_exception_handler as _,
                            Value::new(false),
                        )
                        .into()
                });
                return NativeCallReturn {
                    ret: self.ctx.return_call(handler, [err], Some(self.retk)),
                };
            }
        };
        NativeCallReturn {
            ret: self.ctx.return_call(self.retk, values, None),
        }
    }

    pub fn return_many(self, values: &[Value<'gc>]) -> NativeCallReturn<'gc> {
        NativeCallReturn {
            ret: self
                .ctx
                .return_call(self.retk, values.iter().copied(), None),
        }
    }

    pub fn return_error(self, err: Value<'gc>) -> NativeCallReturn<'gc> {
        NativeCallReturn {
            ret: NativeReturn {
                code: ReturnCode::ReturnErr,
                value: err,
            },
        }
    }

    pub fn throw(self, key: Value<'gc>, args: Value<'gc>) -> NativeCallReturn<'gc> {
        let throw = throw::throw(self.ctx, key, args, self.retk);

        NativeCallReturn { ret: throw }
    }

    pub fn return_call(self, proc: Value<'gc>, args: &[Value<'gc>]) -> NativeCallReturn<'gc> {
        if !is_procedure(proc) {
            return self.wrong_argument_violation(
                "apply",
                "attempt to call non procedure value",
                Some(proc),
                Some(0),
                0,
                &[],
            );
        }
        NativeCallReturn {
            ret: self
                .ctx
                .return_call(proc, args.iter().copied(), Some(self.retk)),
        }
    }

    /// Perform call into `proc` with `args`, using `retk` as return continuation.
    ///
    /// # Safety
    ///
    /// No type-checks are performed nor arity checks.
    // SAFETY: Caller must ensure preconditions are met (see fn docs)
    pub unsafe fn return_call_unsafe(
        self,
        retk: Value<'gc>,
        proc: Value<'gc>,
        args: &[Value<'gc>],
    ) -> NativeCallReturn<'gc> {
        NativeCallReturn {
            ret: self.ctx.return_call(proc, args.iter().copied(), Some(retk)),
        }
    }

    /// Continue to a continuation `cont` with `args`.
    ///
    /// # Safety
    ///
    /// This function is unsafe because continuations can be misused easily.
    // SAFETY: Caller must ensure preconditions are met (see fn docs)
    pub unsafe fn continue_to(
        self,
        cont: Value<'gc>,
        args: &[Value<'gc>],
    ) -> NativeCallReturn<'gc> {
        if !is_continuation(cont) {
            return self.wrong_argument_violation(
                "@continue",
                "attempt to call non continuation value",
                Some(cont),
                Some(0),
                0,
                &[],
            );
        }
        NativeCallReturn {
            ret: self.ctx.return_call(cont, args.iter().copied(), None),
        }
    }

    pub fn call(
        self,
        proc: Value<'gc>,
        args: &[Value<'gc>],
        retk: Value<'gc>,
    ) -> NativeCallReturn<'gc> {
        if !is_procedure(proc) {
            crate::runtime::vm::debug::print_stacktraces_impl(self.ctx);

            return self.wrong_argument_violation(
                "apply",
                "attempt to call non procedure value",
                Some(proc),
                Some(0),
                0,
                &[],
            );
        }

        if !is_continuation(retk) {
            return self.wrong_argument_violation(
                "apply",
                "expected continuation for return continuation",
                Some(retk),
                Some(0),
                0,
                &[],
            );
        }

        NativeCallReturn {
            ret: self.ctx.return_call(proc, args.iter().copied(), Some(retk)),
        }
    }

    pub fn wrong_number_of_arguments_violation(
        self,
        who: &str,
        required_min: usize,
        required_max: Option<usize>,
        argc: usize,
        args: &[Value<'gc>],
    ) -> NativeCallReturn<'gc> {
        let plural = if argc < 2 { "" } else { "s" };

        let message = if let Some(max) = required_max
            && max == required_min
        {
            format!(
                "expected {}, but {} argument{} given",
                required_min, argc, plural
            )
        } else if let Some(max) = required_max {
            if max == required_min + 1 {
                format!(
                    "expected {} or {}, but {} argument{} given",
                    required_min, max, argc, plural
                )
            } else {
                format!(
                    "expected between {} and {}, but {} argument{} given",
                    required_min, max, argc, plural
                )
            }
        } else {
            format!(
                "expected at least {}, but {} argument{} given",
                required_min, argc, plural
            )
        };
        let who = Value::new(Symbol::from_str(self.ctx, who));
        let message = Str::new(*self.ctx, message, true).into();
        if argc == 0 {
            self.raise_assertion_violation(who, message, Value::null())
        } else {
            let mut irritants = Value::null();
            for &arg in args.iter().rev() {
                irritants = Value::cons(self.ctx, arg, irritants);
            }
            self.raise_assertion_violation(who, message, irritants)
        }
    }

    pub fn wrong_number_of_arguments_violation_proc(
        self,
        proc: Value<'gc>,
        required_min: usize,
        required_max: Option<usize>,
        argc: usize,
        args: &[Value<'gc>],
    ) -> NativeCallReturn<'gc> {
        if proc.is::<Closure>() && !proc.downcast::<Closure>().is_continuation() {
            let closure = proc.downcast::<Closure>();
            if closure.meta.get().is_pair() {
                let name = Value::assq(
                    closure.meta.get(),
                    Symbol::from_str(self.ctx, "name").into(),
                )
                .unwrap_or(Value::new(false))
                .to_string();
                return self.wrong_number_of_arguments_violation(
                    &name,
                    required_min,
                    required_max,
                    argc,
                    args,
                );
            }
        }
        let plural = if argc < 2 { "" } else { "s" };

        let message = if let Some(max) = required_max
            && max == required_min
        {
            format!(
                "expected {}, but {} argument{} given",
                required_min, argc, plural
            )
        } else if let Some(max) = required_max {
            if max == required_min + 1 {
                format!(
                    "expected {} or {}, but {} argument{} given",
                    required_min, max, argc, plural
                )
            } else {
                format!(
                    "expected between {} and {}, but {} argument{} given",
                    required_min, max, argc, plural
                )
            }
        } else {
            format!(
                "expected at least {}, but {} argument{} given",
                required_min, argc, plural
            )
        };
        let message = Str::new(*self.ctx, message, true).into();
        if argc == 0 {
            self.raise_assertion_violation(Value::new(false), message, Value::null())
        } else {
            let mut irritants = Value::null();
            for &arg in args.iter().rev() {
                irritants = Value::cons(self.ctx, arg, irritants);
            }
            self.raise_assertion_violation(Value::new(false), message, irritants)
        }
    }

    pub fn raise_assertion_violation(
        self,
        who: Value<'gc>,
        message: Value<'gc>,
        irritant: Value<'gc>,
    ) -> NativeCallReturn<'gc> {
        let assertion_violation = self
            .ctx
            .globals()
            .root_module()
            .get_str(self.ctx, "assertion-violation")
            .unwrap_or_else(|| panic!("pre boot error at {who}: {message} with {irritant}",));
        if irritant != Value::null() {
            self.return_call(assertion_violation, &[who, message, irritant])
        } else {
            self.return_call(assertion_violation, &[who, message])
        }
    }

    pub fn raise_undefined_violation(
        self,
        who: &str,
        message: &str,
        irritants: &[Value<'gc>],
    ) -> NativeCallReturn<'gc> {
        let error = self
            .ctx
            .globals()
            .root_module()
            .get_str(self.ctx, "undefined-violation")
            .expect("pre boot error");
        let who = Value::new(Symbol::from_str(self.ctx, who));
        let message = Str::new(*self.ctx, message, true).into();
        let args = std::iter::once(who)
            .chain(std::iter::once(message))
            .chain(irritants.iter().copied())
            .collect::<Vec<_>>();
        self.return_call(error, &args)
    }

    pub fn implementation_restriction_violation(
        self,
        who: &str,
        message: &str,
        irritants: &[Value<'gc>],
    ) -> NativeCallReturn<'gc> {
        let error = self
            .ctx
            .globals()
            .root_module()
            .get_str(self.ctx, "implementation-restriction-violation")
            .expect("pre boot error");
        let who = Value::new(Symbol::from_str(self.ctx, who));
        let message = Str::new(*self.ctx, message, true).into();
        let args = std::iter::once(who)
            .chain(std::iter::once(message))
            .chain(irritants.iter().copied())
            .collect::<Vec<_>>();
        self.return_call(error, &args)
    }

    pub fn raise_error(
        self,
        who: &str,
        message: &str,
        irritants: &[Value<'gc>],
    ) -> NativeCallReturn<'gc> {
        let error = self
            .ctx
            .globals()
            .root_module()
            .get_str(self.ctx, "error")
            .expect("pre boot error");
        let who = Value::new(Symbol::from_str(self.ctx, who));
        let message = Str::new(*self.ctx, message, true).into();
        let args = std::iter::once(who)
            .chain(std::iter::once(message))
            .chain(irritants.iter().copied())
            .collect::<Vec<_>>();
        self.return_call(error, &args)
    }

    pub fn raise_io_error(
        self,
        err: std::io::Error,
        operation: IoOperation,
        who: &str,
        message: &str,
        filename: Value<'gc>,
    ) -> NativeCallReturn<'gc> {
        let message = if let Some(code) = err.raw_os_error() {
            Str::from_str(*self.ctx, format!("{message} ({code})"))
        } else {
            Str::from_str(*self.ctx, message)
        }
        .into();
        let who = if !who.is_empty() {
            Symbol::from_str(self.ctx, who).into()
        } else {
            Value::new(false)
        };

        match operation {
            IoOperation::Open => match err.raw_os_error() {
                Some(libc::ENOENT) => {
                    let error = self
                        .ctx
                        .globals()
                        .root_module()
                        .get_str(self.ctx, "raise-i/o-file-does-not-exist-error")
                        .expect("pre boot error");
                    self.return_call(error, &[who, message, filename])
                }

                Some(libc::EEXIST) => {
                    let error = self
                        .ctx
                        .globals()
                        .root_module()
                        .get_str(self.ctx, "raise-i/o-file-already-exists-error")
                        .expect("pre boot error");
                    self.return_call(error, &[who, message, filename])
                }

                Some(libc::EROFS) | Some(libc::EISDIR) | Some(libc::ETXTBSY) => {
                    let error = self
                        .ctx
                        .globals()
                        .root_module()
                        .get_str(self.ctx, "raise-i/o-file-is-read-only-error")
                        .expect("pre boot error");
                    self.return_call(error, &[who, message, filename])
                }
                Some(libc::EACCES) => {
                    let error = self
                        .ctx
                        .globals()
                        .root_module()
                        .get_str(self.ctx, "raise-i/o-file-protection-error")
                        .expect("pre boot error");
                    self.return_call(error, &[who, message, filename])
                }

                _ => {
                    let error = self
                        .ctx
                        .globals()
                        .root_module()
                        .get_str(self.ctx, "raise-i/o-error")
                        .expect("pre boot error");
                    self.return_call(error, &[who, message, filename])
                }
            },

            IoOperation::Read => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-read-error")
                    .expect("pre boot error");
                self.return_call(error, &[who, message, filename])
            }

            IoOperation::Write => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-write-error")
                    .expect("pre boot error");
                self.return_call(error, &[who, message, filename])
            }

            IoOperation::Seek => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-seek-error")
                    .expect("pre boot error");
                self.return_call(error, &[who, message, filename])
            }
            _ => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-error")
                    .expect("pre boot error");
                self.return_call(error, &[who, message, filename])
            }
        }
    }

    pub fn raise_io_filesystem_error(
        self,
        who: &str,
        message: &str,
        err: std::io::Error,
        old_filename: Value<'gc>,
        new_filename: Value<'gc>,
    ) -> NativeCallReturn<'gc> {
        let message = if let Some(code) = err.raw_os_error() {
            Str::from_str(*self.ctx, format!("{message} ({code})"))
        } else {
            Str::from_str(*self.ctx, message)
        }
        .into();
        let who = if !who.is_empty() {
            Symbol::from_str(self.ctx, who).into()
        } else {
            Value::new(false)
        };

        match err.kind() {
            ErrorKind::NotFound | ErrorKind::DirectoryNotEmpty => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-file-does-not-exist-error")
                    .expect("pre boot error");
                self.return_call(error, &[who, message, old_filename])
            }

            ErrorKind::AlreadyExists => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-file-already-exists-error")
                    .expect("pre boot error");
                self.return_call(error, &[who, message, new_filename])
            }

            _ => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-error")
                    .expect("pre boot error");
                self.return_call(error, &[who, message, old_filename, new_filename])
            }
        }
    }

    pub fn wrong_argument_violation(
        self,
        who: &str,
        description: &str,
        value: Option<Value<'gc>>,
        position: Option<usize>,
        argc: usize,
        args: &[Value<'gc>],
    ) -> NativeCallReturn<'gc> {
        let message = if let Some(value) = value {
            if let Some(position) = position {
                format!("{description}, got {value}, as argument {}", position + 1)
            } else {
                format!("{description} {value}")
            }
        } else {
            description.to_string()
        };

        let message = Str::new(*self.ctx, &message, true).into();
        let who = Value::new(Symbol::from_str(self.ctx, who));
        if let Some(_) = position
            && argc < 2
        {
            self.raise_assertion_violation(who, message, Value::null())
        } else {
            let mut irritants = Value::null();
            for &arg in args.iter().rev() {
                irritants = Value::cons(self.ctx, arg, irritants);
            }
            self.raise_assertion_violation(who, message, irritants)
        }
    }

    pub fn conversion_error(self, who: &str, conv: ConversionError<'gc>) -> NativeCallReturn<'gc> {
        let rands = self.rands.to_vec();
        match conv {
            ConversionError::TypeMismatch {
                expected,
                found,
                pos,
            } => {
                let description = format!("expected {}, but found", expected);

                self.wrong_argument_violation(
                    who,
                    &description,
                    Some(found),
                    Some(pos),
                    rands.len(),
                    &rands,
                )
            }
            ConversionError::ArityMismatch {
                pos: _,
                expected,
                found: _,
            } => self.wrong_number_of_arguments_violation(
                who,
                expected.min,
                expected.max,
                rands.len(),
                &rands,
            ),
        }
    }
}

/// A native call return value. Just an opaque wrapper over [`NativeReturn`]
/// to enforce proper usage in safe code.
#[repr(transparent)]
pub struct NativeCallReturn<'gc> {
    ret: NativeReturn<'gc>,
}

impl<'gc> NativeCallReturn<'gc> {
    pub fn into_inner(self) -> NativeReturn<'gc> {
        self.ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rsgc::mmtk::util::Address;
    use crate::runtime::Scheme;
    use crate::runtime::value::{CodeArity, CodeBlock};
    use std::{mem::size_of, panic::AssertUnwindSafe};

    extern "C-unwind" fn test_proc<'gc>(
        _ctx: Context<'gc>,
        _rator: Value<'gc>,
        _rands: *const Value<'gc>,
        _num_rands: usize,
        _retk: Value<'gc>,
    ) -> NativeReturn<'gc> {
        NativeReturn {
            code: ReturnCode::ReturnOk,
            value: Value::undefined(),
        }
    }

    #[test]
    fn vm_procedure_checks_use_closure_class_ids() {
        Scheme::new_uninit().enter(|ctx| {
            let code_block = CodeBlock::new_aot(
                ctx,
                Address::from_ptr(test_proc as *const ()),
                CodeArity::new(0),
                false,
                Value::null(),
            );
            let proc = Value::from(Closure::new(ctx, code_block, &[], false));
            let cont = Value::from(Closure::new(ctx, code_block, &[], true));

            assert!(is_procedure(proc));
            assert!(!is_procedure(cont));
            assert!(!is_procedure(Value::null()));
            assert!(is_continuation(cont));
            assert!(!is_continuation(proc));
            assert!(!is_continuation(Value::null()));
        });
    }

    #[test]
    fn nested_scheme_call_guard_restores_state_on_unwind() {
        Scheme::new_uninit().enter(|ctx| {
            let old_runstack = ctx.state().runstack.get();
            assert_eq!(ctx.nest_level(), 0);

            let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
                let _guard = NestedSchemeCallGuard::new(ctx);
                ctx.state().runstack.set(old_runstack + size_of::<Value>());
                assert_eq!(ctx.nest_level(), 1);
                panic!("force unwind across nested Scheme call state");
            }));

            assert!(result.is_err());
            assert_eq!(ctx.nest_level(), 0);
            assert_eq!(ctx.state().runstack.get(), old_runstack);
        });
    }
}
