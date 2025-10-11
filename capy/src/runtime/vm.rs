use std::{
    marker::PhantomData,
    ops::{FromResidual, Try},
    sync::atomic::Ordering,
};

use crate::runtime::{
    Context,
    modules::root_module,
    value::{
        Closure, ConversionError, NativeLocation, NativeReturn, PROCEDURES, ReturnCode, Str,
        Symbol, TryIntoValues, TypeCode16, Value, Vector,
    },
    vm::trampolines::get_trampoline_into_scheme,
};
use rsgc::{Gc, Trace};

pub mod arith;
pub mod base;
pub mod debug;
pub mod errors;
pub mod eval;
pub mod expand;
pub mod ffi;
pub mod hash;
pub mod io;
pub mod libraries;
pub mod list;
pub mod load;
pub mod memoize;
pub mod records;
pub mod strings;
pub mod syntax;
pub mod throw;
pub mod thunks;
pub mod trampolines;
pub mod vector;

/// Perform call into the Scheme code in `rator` with `args`.
///
/// This function performs *nested* call into Scheme. For calling Scheme
/// from native code use [`return_call`](NativeCallContext::return_call) method.
///
/// Once this function is called, current thread nest level is increased by 1
/// and if it is greater than 1, GC safepoint will be ignored until
/// nested level is back to 1.
pub fn call_scheme<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> VMResult<'gc> {
    let procs = PROCEDURES.fetch(&ctx);

    let retk = procs.register_static_cont_closure(
        ctx,
        default_retk,
        NativeLocation::unknown(),
        Value::null(),
    );
    let reth = procs.register_static_cont_closure(
        ctx,
        default_reth,
        NativeLocation::unknown(),
        Value::null(),
    );

    call_scheme_with_k(&ctx, retk.into(), reth.into(), rator, args)
}

/// Call Scheme code with explicit return and error continuations.
pub extern "C" fn call_scheme_with_k<'gc>(
    ctx: &Context<'gc>,
    retk: Value<'gc>,
    reth: Value<'gc>,
    rator: Value<'gc>,
    args: impl IntoIterator<Item = Value<'gc>>,
) -> VMResult<'gc> {
    if !rator.is::<Closure>() {
        return VMResult::Err(rator);
    }
    let old_runstack = ctx.state.runstack.get();
    let nest_level = ctx.state.nest_level.fetch_add(1, Ordering::Relaxed);

    let rands = ctx.state.runstack.get();
    let mut argc = 0;

    unsafe {
        rands.store(retk);
        rands.add(size_of::<Value>()).store(reth);

        for (i, arg) in args.into_iter().enumerate() {
            if rands.add((i + 2) * size_of::<Value>()) >= ctx.state.runstack_end {
                panic!(
                    "runstack overflow: {} >= {}, too many arguments: {}, can fit: {}",
                    rands.add((i + 2) * size_of::<Value>()),
                    ctx.state.runstack_end,
                    i + 2,
                    (ctx.state.runstack_end - ctx.state.runstack_start) / size_of::<Value>()
                );
            }
            if rands.add((i + 2) * size_of::<Value>()) < ctx.state.runstack_start {
                panic!("runstack underflow");
            }
            rands.add((i + 2) * size_of::<Value>()).store(arg);
            argc += 1;
        }

        ctx.state
            .runstack
            .set(rands.add((argc + 2) * size_of::<Value>()));
    }

    let num_rands = argc + 2;

    let f = get_trampoline_into_scheme().to_ptr::<()>();

    unsafe {
        let f: extern "C-unwind" fn(
            &Context,
            Value<'gc>,
            *const Value<'gc>,
            usize,
        ) -> NativeReturn<'gc> = std::mem::transmute(f);

        let val = trampoline(&ctx, rator, rands.to_ptr(), num_rands, f);
        ctx.state.nest_level.store(nest_level, Ordering::Relaxed);
        ctx.state.runstack.set(old_runstack);

        match val.code {
            ReturnCode::Continue => unreachable!("cannot continue into native code"),
            ReturnCode::ReturnErr => VMResult::Err(val.value),
            ReturnCode::ReturnOk => VMResult::Ok(val.value),
            ReturnCode::Yield => {
                ctx.state
                    .saved_call
                    .set(Some(Gc::from_ptr(val.value.bits() as *const _)));

                VMResult::Yield
            }
        }
    }
}

#[inline(never)]
extern "C" fn trampoline<'a>(
    ctx: &Context<'a>,
    rator: Value<'a>,
    rands: *const Value<'a>,
    num_rands: usize,
    f: extern "C-unwind" fn(&Context<'a>, Value<'a>, *const Value<'a>, usize) -> NativeReturn<'a>,
) -> NativeReturn<'a> {
    f(ctx, rator, rands, num_rands)
}

extern "C-unwind" fn default_retk<'gc>(
    ctx: &Context<'gc>,
    _rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
) -> NativeReturn<'gc> {
    let value = if num_rands == 0 {
        Value::undefined()
    } else if num_rands == 1 {
        unsafe { *rands }
    } else {
        let args = unsafe { std::slice::from_raw_parts(rands, num_rands) };
        Vector::from_slice(ctx, args).into()
    };

    NativeReturn {
        code: ReturnCode::ReturnOk,
        value,
    }
}

extern "C-unwind" fn default_reth<'gc>(
    _ctx: &Context<'gc>,
    _rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
) -> NativeReturn<'gc> {
    println!("error occured");
    unsafe {
        let slice = std::slice::from_raw_parts(rands, num_rands);
        if !slice.is_empty() {
            println!(
                "irritants: {}",
                slice
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }
    }
    NativeReturn {
        code: ReturnCode::ReturnErr,
        value: Value::new(42),
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
    /// VM execution was suspended. Caller into the VM is responsible for leaving `Scheme::enter` call temporarily.
    Yield,
}

impl<'gc> FromResidual<Value<'gc>> for VMResult<'gc> {
    fn from_residual(residual: Value<'gc>) -> Self {
        Self::Err(residual)
    }
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
            VMResult::Yield => panic!("cannot use `?` operator on `VMResult::Yield`"),
        }
    }
}

pub struct NativeCallContext<'a, 'gc, R: TryIntoValues<'gc> = Value<'gc>> {
    pub ctx: Context<'gc>,
    rator: Value<'gc>,
    rands: &'a [Value<'gc>],

    pub(crate) retk: Value<'gc>,
    pub(crate) reth: Value<'gc>,
    return_data: PhantomData<R>,
}

impl<'a, 'gc, R: TryIntoValues<'gc>> NativeCallContext<'a, 'gc, R> {
    /// Construct native call context from raw parts.
    ///
    /// # Safety
    ///
    /// Rands must be a valid pointer created by runtime, not arbitrary pointer.
    pub unsafe fn from_raw(
        ctx: &Context<'gc>,
        rator: Value<'gc>,
        rands: *const Value<'gc>,
        num_rands: usize,
        retk: Value<'gc>,
        reth: Value<'gc>,
    ) -> Self {
        Self {
            ctx: *ctx,
            rator,
            rands: unsafe { std::slice::from_raw_parts(rands, num_rands) },
            reth,
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
        self.rator.has_typ16(TypeCode16::CLOSURE_K)
    }

    pub fn return_(self, values: R) -> NativeCallReturn<'gc> {
        let values = match values.try_into_values(self.ctx) {
            Ok(values) => values,
            Err(err) => {
                return NativeCallReturn {
                    ret: self.ctx.return_call(self.reth, [err], None),
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
        let throw = throw::throw(self.ctx, key, args, self.retk, self.reth);

        NativeCallReturn { ret: throw }
    }

    pub fn return_call(self, proc: Value<'gc>, args: &[Value<'gc>]) -> NativeCallReturn<'gc> {
        if !proc.has_typ16(TypeCode16::CLOSURE_PROC) {
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
                .return_call(proc, args.iter().copied(), Some([self.retk, self.reth])),
        }
    }

    /// Perform call into `proc` with `args`, using `retk` and `reth` as return and error continuations.
    ///
    /// # Safety
    ///
    /// No type-checks are performed nor arity checks.
    pub unsafe fn return_call_unsafe(
        self,
        retk: Value<'gc>,
        reth: Value<'gc>,
        proc: Value<'gc>,
        args: &[Value<'gc>],
    ) -> NativeCallReturn<'gc> {
        NativeCallReturn {
            ret: self
                .ctx
                .return_call(proc, args.iter().copied(), Some([retk, reth])),
        }
    }

    /// Continue to a continuation `cont` with `args`.
    ///
    /// # Safety
    ///
    /// This function is unsafe because continuations can be misused easily.
    pub unsafe fn continue_to(
        self,
        cont: Value<'gc>,
        args: &[Value<'gc>],
    ) -> NativeCallReturn<'gc> {
        if !cont.has_typ16(TypeCode16::CLOSURE_K) {
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
        reth: Value<'gc>,
    ) -> NativeCallReturn<'gc> {
        if !proc.has_typ16(TypeCode16::CLOSURE_PROC) {
            return self.wrong_argument_violation(
                "apply",
                "attempt to call non procedure value",
                Some(proc),
                Some(0),
                0,
                &[],
            );
        }

        if !retk.has_typ16(TypeCode16::CLOSURE_K) {
            return self.wrong_argument_violation(
                "apply",
                "expected continuation for return continuation",
                Some(retk),
                Some(0),
                0,
                &[],
            );
        }

        if !reth.has_typ16(TypeCode16::CLOSURE_K) {
            return self.wrong_argument_violation(
                "apply",
                "expected continuation for return continuation",
                Some(reth),
                Some(0),
                0,
                &[],
            );
        }

        NativeCallReturn {
            ret: self
                .ctx
                .return_call(proc, args.iter().copied(), Some([retk, reth])),
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
        let message = Str::new(&self.ctx, message, true).into();
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
        let message = Str::new(&self.ctx, message, true).into();
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
        let assertion_violation = root_module(self.ctx)
            .get_str(self.ctx, "assertion-violation")
            .expect("pre boot error");
        if irritant != Value::null() {
            self.return_call(assertion_violation, &[who, message, irritant])
        } else {
            self.return_call(assertion_violation, &[who, message])
        }
    }

    pub fn raise_error(
        self,
        who: &str,
        message: &str,
        irritants: &[Value<'gc>],
    ) -> NativeCallReturn<'gc> {
        let error = root_module(self.ctx)
            .get_str(self.ctx, "error")
            .expect("pre boot error");
        let who = Value::new(Symbol::from_str(self.ctx, who));
        let message = Str::new(&self.ctx, message, true).into();
        let args = std::iter::once(who)
            .chain(std::iter::once(message))
            .chain(irritants.iter().copied())
            .collect::<Vec<_>>();
        self.return_call(error, &args)
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
                format!("{description} {value}, as argument {}", position + 1)
            } else {
                format!("{description} {value}")
            }
        } else {
            description.to_string()
        };

        let message = Str::new(&self.ctx, &message, true).into();
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
