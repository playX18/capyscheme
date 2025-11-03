use std::{collections::HashMap, sync::LazyLock};

use easy_bitfield::{BitField, BitFieldTrait};
use rsgc::{
    Global, Rootable, alloc::ArrayRef, cell::Lock, collection::Visitor, sync::monitor::Monitor,
};

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
}

impl NativeProc {
    pub fn new<'gc>(ctx: Context<'gc>, proc: Address, is_k: bool) -> Gc<'gc, Self> {
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
    pub direct: Address,
    pub free: Value<'gc>,
    pub meta: Lock<Value<'gc>>,
}

pub type ClosureRef<'gc> = Gc<'gc, Closure<'gc>>;

impl<'gc> Closure<'gc> {
    pub fn new(
        ctx: Context<'gc>,
        code: Address,
        direct: Address,
        free: &[Value<'gc>],
        is_cont: bool,
        meta: Value<'gc>,
    ) -> Gc<'gc, Self> {
        let free = if free.is_empty() {
            Value::new(false)
        } else {
            Vector::from_slice(&ctx, free).into()
        };

        let meta = if meta == Value::new(false) {
            Value::null()
        } else {
            meta
        };

        Gc::new(
            &ctx,
            Self {
                header: ScmHeader::with_type_bits(if is_cont {
                    TypeCode16::CLOSURE_K.0
                } else {
                    TypeCode16::CLOSURE_PROC.0
                }),
                direct,
                code,
                free,
                meta: Lock::new(meta),
            },
        )
    }

    pub fn new_native(
        ctx: Context<'gc>,
        code: Address,
        direct: Address,
        free: &[Value<'gc>],
        is_cont: bool,
        meta: Value<'gc>,
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
                direct,
                free,
                meta: Lock::new(meta),
            },
        )
    }

    pub fn documentation(&self, ctx: Context<'gc>) -> Option<Value<'gc>> {
        let meta = self.meta.get();
        if !meta.is_pair() {
            return None;
        }
        let doc = meta.assq(Symbol::from_str(ctx, "documentation").into());
        doc.map(|d| d.cdr())
    }

    pub fn source(&self, ctx: Context<'gc>) -> Option<(Value<'gc>, u32, u32)> {
        let meta = self.meta.get();
        if !meta.is_pair() {
            return None;
        }

        let src = meta.assq(Symbol::from_str(ctx, "source").into())?;
        let src = src.cdr();

        if src.is::<Vector>() && src.downcast::<Vector>().len() == 3 {
            let vec = src.downcast::<Vector>();
            let file = vec[0].get();
            let line = vec[1].get();
            let column = vec[2].get();

            if file.is::<Str>() && line.is_int32() && column.is_int32() {
                return Some((file, line.as_int32() as u32, column.as_int32() as u32));
            }
        } else if src.is_pair() {
            let file = src.assq(Symbol::from_str(ctx, "file").into())?;
            let line = src.assq(Symbol::from_str(ctx, "line").into())?;
            let column = src.assq(Symbol::from_str(ctx, "column").into())?;

            let file = file.cdr();
            let line = line.cdr();
            let column = column.cdr();

            if file.is::<Str>() && line.is_int32() && column.is_int32() {
                return Some((file, line.as_int32() as u32, column.as_int32() as u32));
            }
        }

        return None;
    }

    pub fn name(&self, ctx: Context<'gc>) -> Option<Value<'gc>> {
        let meta = self.meta.get();
        if !meta.is_pair() {
            return None;
        }
        let name = meta.assq(Symbol::from_str(ctx, "name").into());
        name.map(|n| n.cdr())
    }
}

unsafe impl<'gc> Tagged for Closure<'gc> {
    const TC8: TypeCode8 = TypeCode8::CLOSURE;

    const TC16: &'static [TypeCode16] = &[
        TypeCode16::CLOSURE_PROC,
        TypeCode16::CLOSURE_K,
        TypeCode16::CLOSURE_FOREIGN,
    ];

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

    pub fn is_foreign(&self) -> bool {
        self.header.type_bits() == TypeCode16::CLOSURE_FOREIGN.bits()
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
    ) -> Gc<'gc, NativeProc> {
        let mut registered = self.registered.lock();

        let addr = Address::from_ptr(f as *const ());
        if let Some(&registered) = registered.get(&addr) {
            return registered;
        }

        let proc = NativeProc::new(ctx, addr, true);
        registered.insert(addr, proc);

        proc
    }

    pub fn register_procedure(&self, ctx: Context<'gc>, f: NativeFn<'gc>) -> Gc<'gc, NativeProc> {
        let mut registered = self.registered.lock();
        let addr = Address::from_ptr(f as *const ());
        if let Some(&registered) = registered.get(&addr) {
            return registered;
        }

        let proc = NativeProc::new(ctx, addr, false);
        registered.insert(addr, proc);

        proc
    }

    pub fn register_static_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeFn<'gc>,

        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        let mut closures = self.static_closures.lock();
        if let Some(&clos) = closures.get(&Address::from_ptr(f as *const ())) {
            return clos;
        }
        let proc = self.register_procedure(ctx, f);

        let clos = Closure::new(
            ctx,
            get_trampoline_from_scheme(),
            Address::ZERO,
            &[proc.into()],
            false,
            meta,
        );

        closures.insert(Address::from_ptr(f as *const ()), clos);

        clos
    }

    pub fn register_static_cont_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeContinuation<'gc>,

        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        let mut closures = self.static_closures.lock();
        if let Some(&clos) = closures.get(&Address::from_ptr(f as *const ())) {
            return clos;
        }
        let proc = self.register_continuation(ctx, f);

        let clos = Closure::new(
            ctx,
            get_cont_trampoline_from_scheme(),
            Address::ZERO,
            &[proc.into()],
            true,
            meta,
        );

        closures.insert(Address::from_ptr(f as *const ()), clos);

        clos
    }

    pub fn make_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeFn<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,

        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        let proc = self.register_procedure(ctx, f);
        let mut fv = Vec::with_capacity(1);
        fv.push(proc.into());
        for val in free_vars.into_iter() {
            fv.push(val);
        }
        Closure::new(
            ctx,
            get_trampoline_from_scheme(),
            Address::ZERO,
            &fv,
            false,
            meta,
        )
    }

    pub fn make_cont_closure(
        &self,
        ctx: Context<'gc>,
        f: NativeContinuation<'gc>,
        free_vars: impl IntoIterator<Item = Value<'gc>>,
        meta: Value<'gc>,
    ) -> Gc<'gc, Closure<'gc>> {
        let proc = self.register_continuation(ctx, f);
        let mut fv = Vec::with_capacity(1);
        fv.push(proc.into());
        for val in free_vars.into_iter() {
            fv.push(val);
        }
        Closure::new(
            ctx,
            get_cont_trampoline_from_scheme(),
            Address::ZERO,
            &fv,
            true,
            meta,
        )
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

#[unsafe(export_name = "CAPY_PROCEDURES")]
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
    pub from_procedure: bool,
    pub rands: ArrayRef<'gc, Value<'gc>>,
}
