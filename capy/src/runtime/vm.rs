use std::{
    marker::PhantomData,
    ops::{FromResidual, Try},
};

use crate::runtime::{
    Context,
    value::{
        Closure, NativeReturn, PROCEDURES, ReturnCode, TryIntoValues, TypeCode16, Value, Vector,
    },
    vm::trampolines::get_trampoline_into_scheme,
};
use rsgc::{Gc, Trace};

pub mod arith;
pub mod errors;
pub mod libraries;
pub mod load;
pub mod strings;
pub mod throw;
pub mod thunks;
pub mod trampolines;
pub mod vector;

pub fn call_scheme<'gc>(
    ctx: Context<'gc>,
    rator: Value<'gc>,
    args: &[Value<'gc>],
) -> VMResult<'gc> {
    if !rator.is::<Closure>() {
        return VMResult::Err(rator);
    }

    let procs = PROCEDURES.fetch(&ctx);

    let retk = procs.register_static_cont_closure(ctx, default_retk);
    let reth = procs.register_static_cont_closure(ctx, default_reth);

    let rands = ctx.state().runstack.get();
    unsafe {
        rands.store(retk);
        rands.add(size_of::<Value>()).store(reth);
        for (i, arg) in args.iter().enumerate() {
            rands.add((i + 2) * size_of::<Value>()).store(*arg);
        }

        ctx.state()
            .runstack
            .set(rands.add((args.len() + 2) * size_of::<Value>()));
    }

    let num_rands = args.len() + 2;

    let f = get_trampoline_into_scheme().to_ptr::<()>();

    unsafe {
        let f: extern "C-unwind" fn(
            &Context,
            Value<'gc>,
            *const Value<'gc>,
            usize,
        ) -> NativeReturn<'gc> = std::mem::transmute(f);

        let val = f(&ctx, rator, rands.to_ptr(), num_rands);

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
        Vector::from_slice(&ctx, args).into()
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
        value: Value::new(false),
    }
}

/// A VM execution result.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Trace)]
#[collect(no_drop)]
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

    retk: Value<'gc>,
    reth: Value<'gc>,
    return_data: PhantomData<R>,
}

impl<'a, 'gc, R: TryIntoValues<'gc>> NativeCallContext<'a, 'gc, R> {
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

    pub fn error(
        self,
        key: Value<'gc>,
        subr: &str,
        message: &str,
        args: Value<'gc>,
        rest: Value<'gc>,
    ) -> NativeCallReturn<'gc> {
        NativeCallReturn {
            ret: errors::error(
                self.ctx, key, subr, message, args, rest, self.retk, self.reth,
            ),
        }
    }

    pub fn throw(self, key: Value<'gc>, args: Value<'gc>) -> NativeCallReturn<'gc> {
        let throw = throw::throw(self.ctx, key, args, self.retk, self.reth);

        NativeCallReturn { ret: throw }
    }

    pub fn return_call(self, proc: Value<'gc>, args: &[Value<'gc>]) -> NativeCallReturn<'gc> {
        if !proc.has_typ16(TypeCode16::CLOSURE_PROC) {
            return self.non_applicable(proc);
        }
        NativeCallReturn {
            ret: self
                .ctx
                .return_call(proc, args.iter().copied(), Some([self.retk, self.reth])),
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
            return self.non_applicable(proc);
        }

        if !retk.has_typ16(TypeCode16::CLOSURE_K) {
            return self.non_continuation(retk);
        }

        if !reth.has_typ16(TypeCode16::CLOSURE_K) {
            return self.non_continuation(reth);
        }

        NativeCallReturn {
            ret: self
                .ctx
                .return_call(proc, args.iter().copied(), Some([retk, reth])),
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
