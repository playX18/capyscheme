//! Core virtual machine: call dispatch, stack management, and Scheme ↔ native transitions.

use std::{
    cell::Cell,
    io::ErrorKind,
    ops::{FromResidual, Try},
    sync::atomic::Ordering,
};

use crate::heap::{Trace, object::builtin_class_ids};
use crate::runtime::interp::markers;
use crate::runtime::interp::stack::{self, Slot};
use crate::runtime::{
    MutationContext,
    value::{
        Closure, ConversionError, NativeReturn, PROCEDURES, ReturnCode, Str, Symbol, TryIntoValues,
        Value,
    },
    vm::io::IoOperation,
};
use std::mem::MaybeUninit;

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
pub mod persistent_map;
pub mod persistent_set;
pub mod portio;
pub mod records;
pub mod setjmp;
pub mod strings;
pub mod syntax;
pub mod threading;
pub mod throw;
pub mod thunks;
pub mod vector;

use setjmp::{JmpBuf, SchemeEntryFn, capy_longjmp, capy_with_setjmp};

fn is_procedure(value: Value<'_>) -> bool {
    value
        .class_id()
        .is_some_and(|id| id.bits() == builtin_class_ids::CLOSURE)
}

fn is_continuation(value: Value<'_>) -> bool {
    value.is_current_k()
        || value.is::<stack::Continuation>()
        || value
            .try_as::<Closure>()
            .is_some_and(|closure| closure.is_continuation())
}

/// Perform a nested call into Scheme for `rator` with `args`.
///
/// Prefer CPS [`Call::tail`] from `#[scheme]` leaves when
/// possible (no nest). The **safe** nest API is [`crate::runtime::sni::call_function`].
///
/// # Safety
///
/// Increases [`State::nest_level`](crate::runtime::State::nest_level). While the
/// nested Scheme activation runs, GC may occur at safepoints. The caller must
/// keep every live [`Value`] rooted across this call (e.g. via
/// [`crate::runtime::sni::LocalFrame`]), stack/`register` copies alone are not
/// sufficient under a moving collector.
pub unsafe fn call_scheme<'gc>(
    ctx: MutationContext<'gc>,
    rator: Value<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> ExecutionResult<'gc> {
    // SAFETY: Caller upholds the same rooting contract as this function.
    unsafe { call_scheme_with_k(ctx, Value::current_k(), rator, args) }
}

/// Call Scheme code with an explicit return continuation.
///
/// `k` is either [`Value::current_k`], which returns the values to the
/// Rust caller, or a native continuation closure, which receives them first.
///
/// # Safety
///
/// Same rooting / nest-level contract as [`call_scheme`].
pub unsafe fn call_scheme_with_k<'gc>(
    ctx: MutationContext<'gc>,
    k: Value<'gc>,
    rator: Value<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> ExecutionResult<'gc> {
    if !is_procedure(rator) && !rator.is::<crate::runtime::class::GenericDescriptor>() {
        return ExecutionResult::Err(rator);
    }
    #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
    if crate::runtime::codegen::template_image::runs_template(ctx) {
        // Install the generated markers before any frame below uses them.
        crate::runtime::codegen::template_image::ensure_loaded();
    }
    let args: smallvec::SmallVec<[Value<'gc>; 8]> = args.into_iter().collect();
    // SAFETY: the activation record lives until `pop_activation` below, and
    // the frames are built inside the room `push_activation` reserved.
    unsafe {
        let mut record = MaybeUninit::<stack::Activation<'gc>>::uninit();
        let mut fp = stack::push_activation(ctx, record.as_mut_ptr(), args.len() + 16);
        if !k.is_current_k() {
            // The continuation gets a frame of its own, the callee returns
            // to it through `subr_ret`.
            *fp.add(1) = std::mem::transmute::<Value<'gc>, Value<'static>>(k);
            fp = fp.add(2);
            stack::set_return_address(fp, markers::subr_ret());
        }
        *fp.add(1) = std::mem::transmute::<Value<'gc>, Value<'static>>(rator);
        std::ptr::copy_nonoverlapping(
            args.as_ptr().cast::<Value<'static>>(),
            fp.add(2),
            args.len(),
        );
        let stack_state = &ctx.tc().stack;
        stack_state.fp.set(fp);
        stack_state.sp.set(fp.add(2 + args.len()));

        let old_nest_level = ctx.state().nest_level.fetch_add(1, Ordering::Relaxed);
        let val = run_activation(ctx, fp, args.len());
        ctx.state()
            .nest_level
            .store(old_nest_level, Ordering::Relaxed);
        stack::pop_activation(ctx, record.as_mut_ptr());

        match val.code {
            ReturnCode::Continue => unreachable!("cannot continue into native code"),
            ReturnCode::Raise => unreachable!("Raise is handled inside the activation"),
            ReturnCode::ResumeTemplate => unreachable!("ResumeTemplate never escapes the bail"),
            // `ReturnOne` never escapes a native
            ReturnCode::ReturnOne => unreachable!("ReturnOne escaped its native"),
            ReturnCode::EnterBody => unreachable!("EnterBody escaped its native"),
            ReturnCode::ReturnErr if crate::runtime::sched::scheduler::take_kill_exit() => {
                crate::runtime::sched::scheduler::Killed.unwind()
            }
            ReturnCode::ReturnErr => ExecutionResult::Err(val.value),
            ReturnCode::ReturnOk => ExecutionResult::Ok(val.value),
        }
    }
}

/// Like [`call_scheme`], for callers that already rooted args in local frames.
///
/// # Safety
///
/// Same as [`call_scheme`].
pub unsafe fn call_scheme_with_prepared<'gc>(
    ctx: MutationContext<'gc>,
    rator: Value<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> ExecutionResult<'gc> {
    // SAFETY: caller rooted live Values for the nest.
    unsafe { call_scheme(ctx, rator, args) }
}

/// Continue execution by invoking a Scheme continuation.
///
/// # Safety
///
/// `ctx` must be an active runtime context, `cont` must be a live continuation
/// value for that context, and `args` must contain values valid in `ctx`.
pub unsafe fn continue_to<'gc>(
    ctx: MutationContext<'gc>,
    cont: Value<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> ExecutionResult<'gc> {
    if !cont.is::<Closure>() {
        return ExecutionResult::Err(cont);
    }
    // A continuation closure is entered like a procedure, its stub hands it
    // the values.
    // SAFETY: Caller upholds the rooting contract.
    unsafe { call_scheme_with_k(ctx, Value::current_k(), cont, args) }
}

#[repr(C)]
struct SchemeEnterArgs<'gc> {
    ctx: *const (),
    fp: stack::Slot<'gc>,
    argc: usize,
    result: MaybeUninit<NativeReturn<'static>>,
}

/// C callback invoked under `setjmp` from [`capy_with_setjmp`].
///
/// # Safety
///
/// `data` must point at a live [`SchemeEnterArgs`] for the duration of the call.
unsafe extern "C" fn scheme_enter_callback(data: *mut std::ffi::c_void) {
    // SAFETY: `data` is the `SchemeEnterArgs` stacked in `run_activation`.
    unsafe {
        let args = &mut *data.cast::<SchemeEnterArgs<'_>>();
        let ctx = MutationContext::from_ptr(args.ctx);
        #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
        if crate::runtime::codegen::template_image::runs_template(ctx) {
            args.result
                .write(crate::runtime::codegen::template_image::run_template(
                    ctx, args.fp, args.argc,
                ));
            return;
        }
        args.result.write(crate::runtime::interp::rust_interp::run(
            ctx, args.fp, args.argc,
        ));
    }
}

/// Run the interpreter on the frame at `fp` until the activation exits.
#[inline(never)]
unsafe fn run_activation<'gc>(
    ctx: MutationContext<'gc>,
    fp: stack::Slot<'gc>,
    argc: usize,
) -> NativeReturn<'gc> {
    let state = ctx.state();
    let mut buf = JmpBuf::uninit();
    let buf_ptr = buf.as_mut_ptr();
    let prev = state.exit_jmp.replace(buf_ptr);

    let mut args = SchemeEnterArgs {
        ctx: ctx.as_ptr(),
        fp,
        argc,
        result: MaybeUninit::uninit(),
    };
    let entry: SchemeEntryFn = scheme_enter_callback;
    let result = loop {
        // SAFETY: `buf` and `args` live for this call.
        let rc = unsafe { capy_with_setjmp(buf_ptr, entry, std::ptr::from_mut(&mut args).cast()) };
        if rc == 0 {
            // SAFETY: the callback returned normally, so it wrote a result.
            break unsafe {
                std::mem::transmute::<NativeReturn<'static>, NativeReturn<'gc>>(
                    args.result.assume_init_read(),
                )
            };
        }

        match state.exit_code.get() {
            ReturnCode::Raise => {
                let err = state.exit_value.replace(Value::undefined());
                // SAFETY: whoever raised left the stack state at the frame
                // the error belongs to.
                args.fp =
                    unsafe { crate::runtime::interp::rust_interp::push_raise_frame(ctx, err) };
                args.argc = 1;
            }
            code @ (ReturnCode::ReturnOk | ReturnCode::ReturnErr) => {
                break NativeReturn {
                    code,
                    value: state.exit_value.replace(Value::undefined()),
                };
            }
            ReturnCode::Continue
            | ReturnCode::ResumeTemplate
            | ReturnCode::ReturnOne
            | ReturnCode::EnterBody => {
                unreachable!("cannot continue into native code")
            }
        }
    };
    state.exit_jmp.set(prev);
    result
}

/// Construct an Ok/Err/Raise result on `State` and `longjmp` back to the active Scheme entry.
///
/// # Safety
///
/// Must only be called while `state.exit_jmp` points at a live `setjmp` buffer installed
/// by [`run_activation`].
pub unsafe fn scheme_longjmp<'gc>(
    ctx: MutationContext<'gc>,
    code: ReturnCode,
    value: Value<'gc>,
) -> ! {
    let state = ctx.state();
    state.exit_code.set(code);
    state.exit_value.set(value);
    let buf = state.exit_jmp.get();
    assert!(
        !buf.is_null(),
        "error raised outside of any Scheme activation: {value}"
    );
    // SAFETY: `buf` is the live buffer from the enclosing `run_activation` setjmp.
    unsafe { capy_longjmp(buf, 1) }
}

/// Non-continuable raise from runtime code: unwind to the active Scheme
/// entry, which runs the exception handler above the faulting frame.
pub fn thunk_raise<'gc>(ctx: MutationContext<'gc>, err: Value<'gc>) -> ! {
    // SAFETY: Thunks are only called while an active Scheme entry has installed `exit_jmp`.
    unsafe { scheme_longjmp(ctx, ReturnCode::Raise, err) }
}

pub(crate) extern "C" fn default_exception_handler<'gc>(
    ctx: MutationContext<'gc>,
    sfp: Slot<'gc>,
) -> ReturnCode {
    Call::invoke(ctx, sfp, |call| {
        assert!(
            !call.args.is_empty(),
            "exception handler called with no arguments"
        );
        // The condition is already in place at `sfp[2]`.
        ReturnCode::ReturnErr
    })
}

/// A Scheme VM execution result.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Trace)]
#[collect(no_drop)]
#[repr(C)]
pub enum ExecutionResult<'gc> {
    /// VM executed successfully and value was returned.
    Ok(Value<'gc>),
    /// VM executed, but error was thrown.
    Err(Value<'gc>),
}

impl<'gc> FromResidual<Value<'gc>> for ExecutionResult<'gc> {
    fn from_residual(residual: Value<'gc>) -> Self {
        Self::Err(residual)
    }
}

impl<'gc> std::ops::Residual<Value<'gc>> for Value<'gc> {
    type TryType = ExecutionResult<'gc>;
}

impl<'gc> Try for ExecutionResult<'gc> {
    type Output = Value<'gc>;
    type Residual = Value<'gc>;

    fn from_output(output: Self::Output) -> Self {
        Self::Ok(output)
    }

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            ExecutionResult::Ok(v) => std::ops::ControlFlow::Continue(v),
            ExecutionResult::Err(e) => std::ops::ControlFlow::Break(e),
        }
    }
}

/// What a running `#[scheme]` body staged through `&Call`, if anything.
///
/// Bodies return values directly, a body that must divert control instead
/// stages the request here and returns an ignored [`Value::undefined`], and
/// [`Call::run`] honors the flag over the returned value. The flag lives on the stack-local handle rather
/// than in `Tc`, so a nested `call_scheme` activation cannot disturb it.
#[derive(Debug, Clone, Copy)]
enum Staged<'gc> {
    Call,
    /// The frame was rewritten into a call in place ([`ReturnCode::EnterBody`]).
    Enter,
    Err(Value<'gc>),
}

/// An incoming native call, as seen by a `#[scheme]` body.
///
/// Bodies take `&Call` and return values directly. The few bodies that must
/// divert control (tail calls, calls with an explicit continuation,
/// continuation resumes, throws, errors, multi-value returns) use the
/// staging methods, which record the request, set the staged flag, and
/// return an ignored [`Value::undefined`].
pub struct Call<'a, 'gc> {
    pub ctx: MutationContext<'gc>,
    pub rator: Value<'gc>,
    pub args: &'a [Value<'gc>],
    sfp: Slot<'gc>,
    staged: Cell<Option<Staged<'gc>>>,
}

/// Report a native procedure's panic as a Scheme error raised above the
/// native's frame.
pub(crate) fn native_panic<'gc>(
    ctx: MutationContext<'gc>,
    payload: &Box<dyn std::any::Any + Send + 'static>,
) {
    let message = payload
        .downcast_ref::<String>()
        .cloned()
        .or_else(|| payload.downcast_ref::<&str>().map(|s| s.to_string()))
        .unwrap_or_else(|| String::from("native procedure panicked"));
    ctx.state().exit_value.set(thunks::make_assertion_violation(
        ctx,
        Value::new(false),
        Str::new(ctx, &message, true).into(),
        &[],
    ));
}

impl<'a, 'gc> Call<'a, 'gc> {
    /// A handle for the call on the frame `sfp`: the rator at `sfp[1]`,
    /// the arguments at `sfp[2..]`, counted by `Tc::ac`.
    ///
    /// # Safety
    ///
    /// `sfp` must be the call's live frame and `Tc::ac` its argument count,
    /// both hold at every native entry (the machine stub spills the count,
    /// the driver sets it).
    pub(crate) unsafe fn from_sfp(ctx: MutationContext<'gc>, sfp: Slot<'gc>) -> Self {
        // SAFETY: upheld by the caller (see above).
        unsafe {
            let argc = ctx.tc().ac.get() as usize;
            Self {
                ctx,
                rator: *sfp.add(1),
                args: std::slice::from_raw_parts(sfp.add(2), argc),
                sfp,
                staged: Cell::new(None),
            }
        }
    }

    /// Run a whole native invocation under the panic boundary: `f` covers
    /// argument conversion and the body, and a panic anywhere becomes a
    /// Scheme error raised above the native's frame (`Raise`, with the
    /// condition in `exit_value`).
    pub(crate) fn invoke<F>(ctx: MutationContext<'gc>, sfp: Slot<'gc>, f: F) -> ReturnCode
    where
        F: FnOnce(&Call<'a, 'gc>) -> ReturnCode,
    {
        // SAFETY: a `NativeFn` runs on its call's live frame with the count
        // spilled (the `NativeFn` contract), `from_sfp` itself cannot panic.
        let call = unsafe { Call::from_sfp(ctx, sfp) };

        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| f(&call)));
        match caught {
            Ok(code) => code,
            Err(payload) if payload.is::<crate::runtime::sched::scheduler::Killed>() => {
                crate::runtime::sched::scheduler::begin_kill_exit();
                unsafe { scheme_longjmp(ctx, ReturnCode::ReturnErr, Value::undefined()) }
            }
            Err(payload) => {
                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    native_panic(ctx, &payload)
                })) {
                    Ok(()) => ReturnCode::Raise,
                    Err(_) => std::process::abort(),
                }
            }
        }
    }

    /// Run `body` for this call and lower its outcome to a [`ReturnCode`].
    pub(crate) fn run<R, F>(&self, body: F) -> ReturnCode
    where
        R: TryIntoValues<'gc>,
        F: FnOnce(&Call<'a, 'gc>) -> R,
    {
        let out = body(self);
        match self.staged.get() {
            Some(Staged::Call) => ReturnCode::Continue,
            Some(Staged::Enter) => ReturnCode::EnterBody,
            Some(Staged::Err(err)) => {
                self.write_single(err);
                ReturnCode::ReturnErr
            }
            None => {
                let values = match out.try_into_values(self.ctx) {
                    Ok(values) => values.collect::<smallvec::SmallVec<[Value<'gc>; 8]>>(),
                    Err(err) => {
                        let handler = self.ctx.exception_handler().unwrap_or_else(|| {
                            PROCEDURES
                                .fetch(self.ctx)
                                .register_static_closure(
                                    self.ctx,
                                    default_exception_handler as _,
                                    Value::new(false),
                                )
                                .into()
                        });
                        return self.ctx.stage_tail_call(handler, &[err]);
                    }
                };
                if values.len() == 1 {
                    self.write_single(values[0]);
                    ReturnCode::ReturnOne
                } else {
                    self.ctx.stage_return_values(&values)
                }
            }
        }
    }

    pub(crate) fn finish_flagged(&self) -> ReturnCode {
        match self.staged.get() {
            Some(Staged::Call) => ReturnCode::Continue,
            Some(Staged::Enter) => ReturnCode::EnterBody,
            Some(Staged::Err(err)) => {
                self.write_single(err);
                ReturnCode::ReturnErr
            }
            None => unreachable!("finish_flagged without a staged request"),
        }
    }

    /// Tail-call `body` with `lead` (when present) in front of this call's
    /// arguments, rewriting the frame in place. Returns [`ReturnCode::EnterBody`].
    pub(crate) fn enter_in_place(&self, body: Value<'gc>, lead: Option<Value<'gc>>) -> ReturnCode {
        use crate::runtime::interp::stack;
        let ctx = self.ctx;
        let argc = self.args.len();
        let extra = lead.is_some() as usize;
        // SAFETY: a native owns its top frame, `fp[2..2 + argc]` are the
        // arguments. `reserve` may relocate the frame, so `fp` is reloaded
        // from the stack state before any write.
        unsafe {
            stack::reserve(ctx, argc + extra + 8);
            let stack_state = &ctx.tc().stack;
            let fp = stack_state.fp.get();
            if let Some(lead) = lead {
                // Shift right by one first (`copy` tolerates the overlap):
                // planting the lead first would destroy argument 0.
                std::ptr::copy(fp.add(2), fp.add(3), argc);
                *fp.add(2) = lead;
            }
            *fp.add(1) = body;
            stack_state.sp.set(fp.add(2 + argc + extra));
            ctx.tc().ac.set((argc + extra) as u64);
        }
        ReturnCode::EnterBody
    }

    /// Tail-call `body` with `lead` followed by `args`, which must not alias
    /// this call's frame.
    pub(crate) fn enter_with(
        &self,
        body: Value<'gc>,
        lead: Value<'gc>,
        args: &[Value<'gc>],
    ) -> Value<'gc> {
        use crate::runtime::interp::stack;
        let ctx = self.ctx;
        // SAFETY: as in `enter_in_place`; `args` lives on the heap, so the
        // frame move in `reserve` leaves it intact.
        unsafe {
            stack::reserve(ctx, args.len() + 1 + 8);
            let stack_state = &ctx.tc().stack;
            let fp = stack_state.fp.get();
            *fp.add(1) = body;
            *fp.add(2) = lead;
            std::ptr::copy_nonoverlapping(args.as_ptr(), fp.add(3), args.len());
            stack_state.sp.set(fp.add(3 + args.len()));
            ctx.tc().ac.set((args.len() + 1) as u64);
        }
        self.set_staged(Staged::Enter);
        Value::undefined()
    }

    /// Return `value` from the native
    pub(crate) fn return_one(&self, value: Value<'gc>) -> ReturnCode {
        self.write_single(value);
        ReturnCode::ReturnOne
    }

    /// Write a single result or error value in place at `sfp[2]`.
    fn write_single(&self, value: Value<'gc>) {
        // SAFETY: `sfp` is the call's live frame (`from_sfp` contract), so
        // its value slot is writable for values of this call.
        unsafe {
            *(self.sfp as *mut Value<'gc>).add(2) = value;
        }
    }

    fn set_staged(&self, staged: Staged<'gc>) {
        debug_assert!(self.staged.get().is_none(), "native call staged twice");
        self.staged.set(Some(staged));
    }

    /// Stage a tail call of `proc` with `args` from this call's frame.
    ///
    /// Returns an ignored [`Value::undefined`], the trampoline honors the
    /// staged request over the body's returned value.
    pub fn tail(&self, proc: Value<'gc>, args: &[Value<'gc>]) -> Value<'gc> {
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
        self.set_staged(Staged::Call);
        let ret = self.ctx.stage_tail_call(proc, args);
        debug_assert_eq!(ret, ReturnCode::Continue);
        Value::undefined()
    }

    /// Stage a call of `proc` with `args` that returns to the native
    /// continuation `k` through `subr_ret`.
    ///
    /// Returns an ignored [`Value::undefined`], the trampoline honors the
    /// staged request over the body's returned value.
    pub fn call_with(&self, proc: Value<'gc>, args: &[Value<'gc>], k: Value<'gc>) -> Value<'gc> {
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

        if !is_continuation(k) {
            return self.wrong_argument_violation(
                "apply",
                "expected continuation for return continuation",
                Some(k),
                Some(0),
                0,
                &[],
            );
        }

        self.set_staged(Staged::Call);
        let ret = self.ctx.stage_call_with(proc, args, k);
        debug_assert_eq!(ret, ReturnCode::Continue);
        Value::undefined()
    }

    /// Continue to a continuation `cont` with `args`.
    ///
    /// Returns an ignored [`Value::undefined`], the trampoline honors the
    /// staged request over the body's returned value.
    ///
    /// # Safety
    ///
    /// This function is unsafe because continuations can be misused easily.
    pub unsafe fn continue_to(&self, cont: Value<'gc>, args: &[Value<'gc>]) -> Value<'gc> {
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
        self.set_staged(Staged::Call);
        let ret = self.ctx.stage_resume(cont, args);
        debug_assert_eq!(ret, ReturnCode::Continue);
        Value::undefined()
    }

    /// Throw to the `throw` handler with `key` and `args`.
    ///
    /// Returns an ignored [`Value::undefined`], the trampoline honors the
    /// staged request over the body's returned value.
    pub fn throw(&self, key: Value<'gc>, args: Value<'gc>) -> Value<'gc> {
        self.set_staged(Staged::Call);
        let ret = throw::throw(self.ctx, key, args);
        debug_assert_eq!(ret, ReturnCode::Continue);
        Value::undefined()
    }

    /// Fail this call with `err`, ending the activation with an error.
    ///
    /// Returns an ignored [`Value::undefined`], the trampoline honors the
    /// staged request over the body's returned value.
    pub fn err(&self, err: Value<'gc>) -> Value<'gc> {
        self.set_staged(Staged::Err(err));
        Value::undefined()
    }

    /// Return `vals` (any count) to this call's caller.
    ///
    /// Single values should be returned directly instead, this is for
    /// dynamically-sized multi-value returns. Returns an ignored
    /// [`Value::undefined`], the trampoline honors the staged request over
    /// the body's returned value.
    pub fn values(&self, vals: &[Value<'gc>]) -> Value<'gc> {
        self.set_staged(Staged::Call);
        let ret = self.ctx.stage_return_values(vals);
        debug_assert_eq!(ret, ReturnCode::Continue);
        Value::undefined()
    }

    pub fn wrong_number_of_arguments_violation(
        &self,
        who: &str,
        required_min: usize,
        required_max: Option<usize>,
        argc: usize,
        args: &[Value<'gc>],
    ) -> Value<'gc> {
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
        let message = Str::new(self.ctx, message, true).into();
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

    pub fn raise_assertion_violation(
        &self,
        who: Value<'gc>,
        message: Value<'gc>,
        irritant: Value<'gc>,
    ) -> Value<'gc> {
        let assertion_violation = self
            .ctx
            .globals()
            .root_module()
            .get_str(self.ctx, "assertion-violation")
            .unwrap_or_else(|| panic!("pre boot error at {who}: {message} with {irritant}",));
        if irritant != Value::null() {
            self.tail(assertion_violation, &[who, message, irritant])
        } else {
            self.tail(assertion_violation, &[who, message])
        }
    }

    pub fn raise_undefined_violation(
        &self,
        who: &str,
        message: &str,
        irritants: &[Value<'gc>],
    ) -> Value<'gc> {
        let error = self
            .ctx
            .globals()
            .root_module()
            .get_str(self.ctx, "undefined-violation")
            .expect("pre boot error");
        let who = Value::new(Symbol::from_str(self.ctx, who));
        let message = Str::new(self.ctx, message, true).into();
        let args = std::iter::once(who)
            .chain(std::iter::once(message))
            .chain(irritants.iter().copied())
            .collect::<Vec<_>>();
        self.tail(error, &args)
    }

    pub fn implementation_restriction_violation(
        &self,
        who: &str,
        message: &str,
        irritants: &[Value<'gc>],
    ) -> Value<'gc> {
        let error = self
            .ctx
            .globals()
            .root_module()
            .get_str(self.ctx, "implementation-restriction-violation")
            .expect("pre boot error");
        let who = Value::new(Symbol::from_str(self.ctx, who));
        let message = Str::new(self.ctx, message, true).into();
        let args = std::iter::once(who)
            .chain(std::iter::once(message))
            .chain(irritants.iter().copied())
            .collect::<Vec<_>>();
        self.tail(error, &args)
    }

    pub fn raise_error(&self, who: &str, message: &str, irritants: &[Value<'gc>]) -> Value<'gc> {
        let error = self
            .ctx
            .globals()
            .root_module()
            .get_str(self.ctx, "error")
            .expect("pre boot error");
        let who = Value::new(Symbol::from_str(self.ctx, who));
        let message = Str::new(self.ctx, message, true).into();
        let args = std::iter::once(who)
            .chain(std::iter::once(message))
            .chain(irritants.iter().copied())
            .collect::<Vec<_>>();
        self.tail(error, &args)
    }

    pub fn raise_io_error(
        &self,
        err: std::io::Error,
        operation: IoOperation,
        who: &str,
        message: &str,
        filename: Value<'gc>,
    ) -> Value<'gc> {
        let message = if let Some(code) = err.raw_os_error() {
            Str::from_str(self.ctx, format!("{message} ({code})"))
        } else {
            Str::from_str(self.ctx, message)
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
                    self.tail(error, &[who, message, filename])
                }

                Some(libc::EEXIST) => {
                    let error = self
                        .ctx
                        .globals()
                        .root_module()
                        .get_str(self.ctx, "raise-i/o-file-already-exists-error")
                        .expect("pre boot error");
                    self.tail(error, &[who, message, filename])
                }

                Some(libc::EROFS) | Some(libc::EISDIR) | Some(libc::ETXTBSY) => {
                    let error = self
                        .ctx
                        .globals()
                        .root_module()
                        .get_str(self.ctx, "raise-i/o-file-is-read-only-error")
                        .expect("pre boot error");
                    self.tail(error, &[who, message, filename])
                }
                Some(libc::EACCES) => {
                    let error = self
                        .ctx
                        .globals()
                        .root_module()
                        .get_str(self.ctx, "raise-i/o-file-protection-error")
                        .expect("pre boot error");
                    self.tail(error, &[who, message, filename])
                }

                _ => {
                    let error = self
                        .ctx
                        .globals()
                        .root_module()
                        .get_str(self.ctx, "raise-i/o-error")
                        .expect("pre boot error");
                    self.tail(error, &[who, message, filename])
                }
            },

            IoOperation::Read => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-read-error")
                    .expect("pre boot error");
                self.tail(error, &[who, message, filename])
            }

            IoOperation::Write => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-write-error")
                    .expect("pre boot error");
                self.tail(error, &[who, message, filename])
            }

            IoOperation::Seek => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-seek-error")
                    .expect("pre boot error");
                self.tail(error, &[who, message, filename])
            }
            _ => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-error")
                    .expect("pre boot error");
                self.tail(error, &[who, message, filename])
            }
        }
    }

    pub fn raise_io_filesystem_error(
        &self,
        who: &str,
        message: &str,
        err: std::io::Error,
        old_filename: Value<'gc>,
        new_filename: Value<'gc>,
    ) -> Value<'gc> {
        let message = if let Some(code) = err.raw_os_error() {
            Str::from_str(self.ctx, format!("{message} ({code})"))
        } else {
            Str::from_str(self.ctx, message)
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
                self.tail(error, &[who, message, old_filename])
            }

            ErrorKind::AlreadyExists => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-file-already-exists-error")
                    .expect("pre boot error");
                self.tail(error, &[who, message, new_filename])
            }

            _ => {
                let error = self
                    .ctx
                    .globals()
                    .root_module()
                    .get_str(self.ctx, "raise-i/o-error")
                    .expect("pre boot error");
                self.tail(error, &[who, message, old_filename, new_filename])
            }
        }
    }

    pub fn wrong_argument_violation(
        &self,
        who: &str,
        description: &str,
        value: Option<Value<'gc>>,
        position: Option<usize>,
        argc: usize,
        args: &[Value<'gc>],
    ) -> Value<'gc> {
        let message = if let Some(value) = value {
            if let Some(position) = position {
                format!("{description}, got {value}, as argument {}", position + 1)
            } else {
                format!("{description} {value}")
            }
        } else {
            description.to_string()
        };

        let message = Str::new(self.ctx, &message, true).into();
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

    pub fn conversion_error(&self, who: &str, conv: ConversionError<'gc>) -> Value<'gc> {
        // No flag of its own: the delegated violation below stages.
        let args = self.args;
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
                    args.len(),
                    args,
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
                args.len(),
                args,
            ),
        }
    }
}
