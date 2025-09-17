use std::{borrow::Cow, collections::HashMap, sync::LazyLock};

use easy_bitfield::{BitField, BitFieldTrait};
use rsgc::{Global, Rootable, alloc::ArrayRef, collection::Visitor, sync::monitor::Monitor};

use crate::runtime::{
    Context,
    vm::trampolines::{get_cont_trampoline_from_scheme, get_trampoline_from_scheme},
};

use super::*;
pub enum Return<'gc> {
    Continue(Value<'gc>),
    Err(Value<'gc>),
}

pub type NativeFn<'gc> = extern "C-unwind" fn(
    ctx: &Context<'gc>,
    rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
    retk: Value<'gc>,
    reth: Value<'gc>,
) -> NativeReturn<'gc>;

pub type NativeContinuation<'gc> = extern "C-unwind" fn(
    ctx: &Context<'gc>,
    rator: Value<'gc>,
    rands: *const Value<'gc>,
    num_rands: usize,
) -> NativeReturn<'gc>;

pub type ClosureNativeFlag = BitField<u64, bool, { TypeBits::NEXT_BIT }, 1, false>;
pub type ClosureContinuationFlag = BitField<u64, bool, { TypeBits::NEXT_BIT + 1 }, 1, false>;
#[repr(C)]
pub struct NativeProc {
    pub header: ScmHeader,
    pub proc: Address,
    pub loc: NativeLocation,
}

impl NativeProc {
    pub fn new<'gc>(
        ctx: Context<'gc>,
        proc: Address,
        is_k: bool,
        loc: NativeLocation,
    ) -> Gc<'gc, Self> {
        let tc = if is_k {
            TypeCode16::NATIVE_K
        } else {
            TypeCode16::NATIVE_PROC
        };
        Gc::new(
            &ctx,
            NativeProc {
                header: ScmHeader::with_type_bits(tc.0),
                proc,
                loc,
            },
        )
    }
}

unsafe impl Tagged for NativeProc {
    const TC8: TypeCode8 = TypeCode8::NATIVE_PROCEDURE;
    const TC16: &[TypeCode16] = &[TypeCode16::NATIVE_PROC, TypeCode16::NATIVE_K];
    const TYPE_NAME: &'static str = "native-procedure";
}

unsafe impl Trace for NativeProc {
    unsafe fn trace(&mut self, _vis: &mut Visitor) {}

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}
}

#[repr(C)]
pub struct Closure<'gc> {
    pub header: ScmHeader,
    pub code: Address,
    pub free: Value<'gc>,
    pub meta: Value<'gc>,
}

pub type ClosureRef<'gc> = Gc<'gc, Closure<'gc>>;

impl<'gc> Closure<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        code: Address,
        free: &[Value<'gc>],
        is_cont: bool,
    ) -> Gc<'gc, Self> {
        let free = if free.is_empty() {
            Value::new(false)
        } else {
            Vector::from_slice(&ctx, free).into()
        };

        Gc::new(
            &ctx,
            Self {
                header: ScmHeader::with_type_bits(if is_cont {
                    TypeCode16::CLOSURE_K.0
                } else {
                    TypeCode16::CLOSURE_PROC.0
                }),
                code,
                free,
                meta: Value::new(false),
            },
        )
    }

    pub fn new_native(
        ctx: Context<'gc>,
        code: Address,
        free: &[Value<'gc>],
        is_cont: bool,
    ) -> Gc<'gc, Self> {
        let free = if free.is_empty() {
            Value::new(false)
        } else {
            Vector::from_slice(&ctx, free).into()
        };

        let mut header = ScmHeader::with_type_bits(if is_cont {
            TypeCode16::CLOSURE_K.0
        } else {
            TypeCode16::CLOSURE_PROC.0
        });

        header.word |= ClosureNativeFlag::encode(true);

        Gc::new(
            &ctx,
            Self {
                header,
                code,
                free,
                meta: Value::new(false),
            },
        )
    }
}

unsafe impl<'gc> Tagged for Closure<'gc> {
    const TC8: TypeCode8 = TypeCode8::CLOSURE;

    const TC16: &'static [TypeCode16] = &[TypeCode16::CLOSURE_PROC, TypeCode16::CLOSURE_K];

    const TYPE_NAME: &'static str = "procedure";
}

unsafe impl<'gc> Trace for Closure<'gc> {
    unsafe fn trace(&mut self, vis: &mut Visitor) {
        vis.trace(&mut self.free);
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> Closure<'gc> {
    pub fn is_continuation(&self) -> bool {
        self.header.type_bits() == TypeCode16::CLOSURE_K.bits()
    }
}

pub struct Procedures<'gc> {
    registered: Monitor<HashMap<Address, Gc<'gc, NativeProc>>>,
    static_closures: Monitor<HashMap<Address, Gc<'gc, Closure<'gc>>>>,
}

impl<'gc> Procedures<'gc> {
    pub fn register_continuation(
        &self,
        ctx: Context<'gc>,
        f: NativeContinuation<'gc>,
        loc: NativeLocation,
    ) -> Gc<'gc, NativeProc> {
        let mut registered = self.registered.lock();

        let addr = Address::from_ptr(f as *const ());
        if let Some(&registered) = registered.get(&addr) {
            return registered;
        }

        let proc = NativeProc::new(ctx, addr, true, loc);
        registered.insert(addr, proc);

        proc
    }

    pub fn register_procedure(
        &self,
        ctx: Context<'gc>,
        f: NativeFn<'gc>,
        loc: NativeLocation,
    ) -> Gc<'gc, NativeProc> {
        let mut registered = self.registered.lock();
        let addr = Address::from_ptr(f as *const ());
        if let Some(&registered) = registered.get(&addr) {
            return registered;
        }

        let proc = NativeProc::new(ctx, addr, false, loc);
        registered.insert(addr, proc);

        proc
    }

    pub fn register_static_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeFn<'gc>,
        loc: NativeLocation,
    ) -> Gc<'gc, Closure<'gc>> {
        let mut closures = self.static_closures.lock();
        if let Some(&clos) = closures.get(&Address::from_ptr(f as *const ())) {
            return clos;
        }
        let proc = self.register_procedure(ctx, f, loc);

        let clos = Closure::new(ctx, get_trampoline_from_scheme(), &[proc.into()], false);

        closures.insert(Address::from_ptr(f as *const ()), clos);

        clos
    }

    pub fn register_static_cont_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeContinuation<'gc>,
        loc: NativeLocation,
    ) -> Gc<'gc, Closure<'gc>> {
        let mut closures = self.static_closures.lock();
        if let Some(&clos) = closures.get(&Address::from_ptr(f as *const ())) {
            return clos;
        }
        let proc = self.register_continuation(ctx, f, loc);

        let clos = Closure::new(ctx, get_cont_trampoline_from_scheme(), &[proc.into()], true);

        closures.insert(Address::from_ptr(f as *const ()), clos);

        clos
    }

    pub fn make_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeFn<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        loc: NativeLocation,
    ) -> Gc<'gc, Closure<'gc>> {
        let proc = self.register_procedure(ctx, f, loc);
        let mut fv = Vec::with_capacity(1);
        fv.push(proc.into());
        for val in free_vars.into_iter() {
            fv.push(val);
        }
        Closure::new(ctx, get_trampoline_from_scheme(), &fv, false)
    }

    pub fn make_cont_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeContinuation<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        loc: NativeLocation,
    ) -> Gc<'gc, Closure<'gc>> {
        let proc = self.register_continuation(ctx, f, loc);
        let mut fv = Vec::with_capacity(1);
        fv.push(proc.into());
        for val in free_vars.into_iter() {
            fv.push(val);
        }
        Closure::new(ctx, get_cont_trampoline_from_scheme(), &fv, true)
    }
}

unsafe impl<'gc> Trace for Procedures<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for (_, proc) in self.registered.get_mut().iter_mut() {
            visitor.trace(proc);
        }

        for (_, clos) in self.static_closures.get_mut().iter_mut() {
            visitor.trace(clos);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

pub static PROCEDURES: LazyLock<Global<Rootable!(Procedures<'_>)>> = LazyLock::new(|| {
    Global::new(Procedures {
        registered: Monitor::new(HashMap::new()),
        static_closures: Monitor::new(HashMap::new()),
    })
});

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum ReturnCode {
    ReturnOk = 0,
    ReturnErr = 1,
    Yield = 2,
    Continue = 3,
}

#[repr(C)]
pub struct NativeReturn<'gc> {
    pub code: ReturnCode,
    pub value: Value<'gc>,
}

/// A saved call to Scheme code. This type is used to represent a suspended call
/// to a Scheme function, allowing it to be resumed later. Main user is yielding for GC.
#[derive(Trace, Clone, Copy)]
#[collect(no_drop)]
#[repr(C)]
pub struct SavedCall<'gc> {
    pub rator: Value<'gc>,
    pub rands: ArrayRef<'gc, Value<'gc>>,
}

#[macro_export]
macro_rules! native_fn {
    ($register_fn: ident: $(
        $(#[$m: meta])*
        $v: vis ($l: literal) fn $name: ident <$gc:lifetime>($ctx: ident$(,)? $($arg: ident: $arg_ty:ty),*) -> $ret: ty $b: block)*) => {
        paste::paste! {
            $(
                pub const [<$name: upper _DOC>]: $crate::runtime::value::proc::NativeLocation = $crate::runtime::value::proc::NativeLocation {
                    file: ::std::borrow::Cow::Borrowed(file!()),
                    name: ::std::borrow::Cow::Borrowed($l),
                    line: line!(),
                };
                $(#[$m])*
                #[allow(unused_parens)]
                #[allow(clippy::macro_metavars_in_unsafe)]
                $v extern "C-unwind" fn [<c_ $name _raw>] <$gc>(
                    ctx: &$crate::runtime::prelude::Context<$gc>,
                    rator: $crate::runtime::prelude::Value<$gc>,
                    rands: *const $crate::runtime::prelude::Value<$gc>,
                    num_rands: usize,
                    retk: $crate::runtime::prelude::Value<$gc>,
                    reth: $crate::runtime::prelude::Value<$gc>
                ) -> $crate::runtime::prelude::NativeReturn<'gc> {

                    unsafe {
                        let rands_ = std::slice::from_raw_parts(rands, num_rands);
                        let nctx = $crate::runtime::prelude::NativeCallContext::<$ret>::from_raw(
                            ctx,
                            rator,
                            rands,
                            num_rands,
                            retk,
                            reth
                        );

                        type Args<$gc> = ($($arg_ty),*);


                        let arity = <Args<$gc> as $crate::runtime::prelude::FromValues>::ARITY;

                        if !arity.is_valid(num_rands) {
                            let rands = nctx.rands().to_vec();
                            return nctx.wrong_number_of_arguments_violation(
                                $l,
                                arity.min,
                                arity.max,
                                num_rands,
                                &rands
                            ).into_inner();
                        }

                        let ($($arg),*): ($($arg_ty),*) = match <($($arg_ty),*) as $crate::runtime::prelude::FromValues>::from_values(nctx.ctx, &mut 0, rands_) {
                            Ok(args) => args,
                            Err(err) => return nctx.conversion_error($l, err).into_inner()
                        };

                        let return_value = $name(nctx, $($arg),*);

                        return_value.into_inner()
                    }
                }

                $(#[$m])*
                #[inline(always)]
                $v fn $name<$gc>($ctx: $crate::runtime::prelude::NativeCallContext<'_, $gc, $ret>, $($arg: $arg_ty),*) -> $crate::runtime::prelude::NativeCallReturn<$gc> {
                    $b
                }


                $v static [<STATIC_ $name: upper _ CLOSURE>]: ::std::sync::OnceLock<$crate::rsgc::global::Global<$crate::rsgc::Rootable!(
                    $crate::runtime::prelude::ClosureRef<'_>
                )>> = ::std::sync::OnceLock::new();

                $v fn [<get_ $name _static_closure>]<'gc>(ctx: $crate::runtime::prelude::Context<'gc>) -> $crate::runtime::prelude::ClosureRef<'gc> {
                    *[<STATIC_ $name: upper _CLOSURE>].get_or_init(|| {
                        $crate::rsgc::global::Global::new($crate::runtime::prelude::PROCEDURES.fetch(&ctx).register_static_closure(ctx, [<c_ $name _raw>], [<$name: upper _DOC>].clone()))
                    })
                        .fetch(&ctx)
                }

                $v fn [<make_ $name _closure>]<'gc>(ctx: $crate::runtime::prelude::Context<'gc>, vars: impl IntoIterator<Item = $crate::runtime::prelude::Value<'gc>>) -> $crate::runtime::prelude::ClosureRef<'gc> {
                    $crate::runtime::prelude::PROCEDURES.fetch(&ctx).make_closure(ctx, [<c_ $name _raw>], vars, [<$name: upper _DOC>].clone())
                }



                static [<$name: upper _LOC>]: ::std::sync::OnceLock<$crate::rsgc::global::Global< $crate::rsgc::Rootable!($crate::runtime::prelude::VariableRef<'_>) >> = ::std::sync::OnceLock::new();

                pub fn [<$name _loc>]<'gc>(ctx: $crate::runtime::prelude::Context<'gc>) -> Option<$crate::runtime::prelude::VariableRef<'gc>> {
                    [<$name: upper _LOC>].get().map(|g| *g.fetch(&ctx))
                }
        )*

            fn $register_fn<'gc>(ctx: $crate::runtime::prelude::Context<'gc>) {
                $(
                    let closure = [<get_ $name _static_closure>](ctx);
                    let _ = [<$name:upper _LOC>].set($crate::rsgc::global::Global::new($crate::runtime::modules::define(ctx, $l, closure.into())));
                )*
            }

        }
    };
    (@get_docs ($($docs:tt)*) #[doc = $doc: literal] $(#[$rest:meta])*) => {
        native_fn!(@get_docs ($($docs)* $doc,) $(#[$rest])*)
    };

    (@get_docs ($($docs:tt)*) $other: meta $(#[$rest:meta])*) => {
        native_fn!(@get_docs ($($docs)*) $(#[$rest])*)
    };

    (@get_docs ($($docs:tt)*)) => {
        $($docs)*
    };
}
#[macro_export]
macro_rules! native_cont {
    ($register_fn: ident: $(
        $(#[$m: meta])*
        $v: vis ($l: literal) fn $name: ident <$gc:lifetime>($ctx: ident$(,)? $($arg: ident: $arg_ty:ty),*) -> $ret: ty $b: block)*) => {
        paste::paste! {
            $(
                pub const [<$name: upper _DOC>]: $crate::runtime::value::proc::NativeLocation = $crate::runtime::value::proc::NativeLocation {
                    file: ::std::borrow::Cow::Borrowed(file!()),
                    name: ::std::borrow::Cow::Borrowed($l),
                    line: line!(),
                };
                $(#[$m])*
                #[allow(unused_parens)]
                #[allow(clippy::macro_metavars_in_unsafe)]
                $v extern "C-unwind" fn [<c_ $name _raw>] <$gc>(
                    ctx: &$crate::runtime::prelude::Context<$gc>,
                    rator: $crate::runtime::prelude::Value<$gc>,
                    rands: *const $crate::runtime::prelude::Value<$gc>,
                    num_rands: usize,
                ) -> $crate::runtime::prelude::NativeReturn<'gc> {
                    unsafe {
                        let rands_ = std::slice::from_raw_parts(rands, num_rands);
                        let nctx = $crate::runtime::prelude::NativeCallContext::<$ret>::from_raw(
                            ctx,
                            rator,
                            rands,
                            num_rands,
                            $crate::runtime::prelude::Value::new(false),
                            $crate::runtime::prelude::Value::new(false)
                        );

                        type Args<$gc> = ($($arg_ty),*);


                        let arity = <Args<$gc> as $crate::runtime::prelude::FromValues>::ARITY;

                        if !arity.is_valid(num_rands) {
                            todo!("arity mismatch")
                        }

                        let ($($arg),*): ($($arg_ty),*) = match <($($arg_ty),*) as $crate::runtime::prelude::FromValues>::from_values(nctx.ctx, &mut 0, rands_) {
                            Ok(args) => args,
                            Err(_) => unreachable!("Checked beforehand")
                        };

                        let return_value = $name(nctx, $($arg),*);

                        return_value.into_inner()
                    }
                }

                $(#[$m])*
                #[inline(always)]
                $v fn $name<$gc>($ctx: $crate::runtime::prelude::NativeCallContext<'_, $gc, $ret>, $($arg: $arg_ty),*) -> $crate::runtime::prelude::NativeCallReturn<$gc> {
                    $b
                }


                $v static [<STATIC_ $name: upper _ CLOSURE>]: ::std::sync::OnceLock<$crate::rsgc::global::Global<$crate::rsgc::Rootable!(
                    $crate::runtime::prelude::ClosureRef<'_>
                )>> = ::std::sync::OnceLock::new();

                $v fn [<get_ $name _static_closure>]<'gc>(ctx: $crate::runtime::prelude::Context<'gc>) -> $crate::runtime::prelude::ClosureRef<'gc> {
                    *[<STATIC_ $name: upper _CLOSURE>].get_or_init(|| {
                        $crate::rsgc::global::Global::new($crate::runtime::prelude::PROCEDURES.fetch(&ctx).register_static_cont_closure(ctx, [<c_ $name _raw>], [<$name: upper _DOC>].clone()))
                    })
                        .fetch(&ctx)
                }

                $v fn [<make_ $name _closure>]<'gc>(ctx: $crate::runtime::prelude::Context<'gc>, vars: impl IntoIterator<Item = $crate::runtime::prelude::Value<'gc>>) -> $crate::runtime::prelude::ClosureRef<'gc> {
                    $crate::runtime::prelude::PROCEDURES.fetch(&ctx).make_cont_closure(ctx, [<c_ $name _raw>], vars, [<$name: upper _DOC>].clone())
                }



                static [<$name: upper _LOC>]: ::std::sync::OnceLock<$crate::rsgc::global::Global< $crate::rsgc::Rootable!($crate::runtime::prelude::VariableRef<'_>) >> = ::std::sync::OnceLock::new();

                pub fn [<$name _loc>]<'gc>(ctx: $crate::runtime::prelude::Context<'gc>) -> Option<$crate::runtime::prelude::VariableRef<'gc>> {
                    [<$name: upper _LOC>].get().map(|g| *g.fetch(&ctx))
                }
        )*

            fn $register_fn<'gc>(ctx: $crate::runtime::prelude::Context<'gc>) {
                $(
                    let closure = [<get_ $name _static_closure>](ctx);
                    let _ = [<$name:upper _LOC>].set($crate::rsgc::global::Global::new($crate::runtime::modules::define(ctx, $l, closure.into())));
                )*
            }

        }
    };
    (@get_docs ($($docs:tt)*) #[doc = $doc: literal] $(#[$rest:meta])*) => {
        native_fn!(@get_docs ($($docs)* $doc,) $(#[$rest])*)
    };

    (@get_docs ($($docs:tt)*) $other: meta $(#[$rest:meta])*) => {
        native_fn!(@get_docs ($($docs)*) $(#[$rest])*)
    };

    (@get_docs ($($docs:tt)*)) => {
        $($docs)*
    };
}

#[derive(Clone)]
pub struct NativeLocation {
    pub file: Cow<'static, str>,
    pub name: Cow<'static, str>,
    pub line: u32,
}

impl NativeLocation {
    pub fn unknown() -> Self {
        Self {
            file: Cow::Borrowed("<unknown>"),
            name: Cow::Borrowed("<unknown>"),
            line: 0,
        }
    }
}
