#![allow(dead_code, unused_variables)]
use crate::runtime::{
    Context,
    fasl::FASLReader,
    modules::{Module, root_module},
    value::{
        Boxed, Closure, Pair, SavedCall, ScmHeader, Str, Symbol, Tuple, TypeCode8, TypeCode16,
        Value, Vector,
    },
    vm::{VMResult, call_scheme, debug},
};
use crate::{
    compiler::ssa::{SSABuilder, traits::IntoSSA},
    runtime::value::{IntoValue, Number},
};
use cranelift_codegen::ir::InstBuilder;
use rsgc::{
    Gc,
    alloc::Array,
    mmtk::{AllocationSemantics, util::Address},
    object::{VTable, VTableOf},
};
use std::{alloc::Layout, cmp::Ordering};

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ThunkResult<'gc> {
    pub code: usize,
    pub value: Value<'gc>,
}

macro_rules! thunks {
    ($gl: lifetime: $(
        $v: vis fn $name : ident($($arg: ident : $t: ty),*) -> $ret: ty $b : block
    )*) => {
        $(
            #[unsafe(export_name=concat!("capy_", stringify!($name)))]
            $v extern "C-unwind" fn $name<$gl>($($arg: $t),*) -> $ret $b
        )*

        pub struct Thunks {
            $(
                pub $name: cranelift_module::FuncId
            ),*
        }

        pub struct ImportedThunks {
            $(
                pub $name: cranelift_codegen::ir::entities::FuncRef
            ),*
        }

        impl Thunks {
            pub fn new<$gl, M: cranelift_module::Module>(
                module: &mut M,
            ) -> Self {
                let callconv = cranelift_codegen::isa::CallConv::SystemV;
                let mut sig = cranelift_codegen::ir::Signature::new(callconv);
                $(
                    $(
                        for ty in < $t as compiler::PrimType>::clif_type() {
                            sig.params.push(cranelift_codegen::ir::AbiParam::new(ty));
                        }
                    )*

                    for ty in <$ret as compiler::PrimType>::clif_type() {
                        sig.returns.push(cranelift_codegen::ir::AbiParam::new(ty));
                    }

                    let $name = module.declare_function(concat!("capy_", stringify!($name)), cranelift_module::Linkage::Import, &sig).unwrap();
                    sig.clear(callconv);
                )*

                Self {
                    $($name),*
                }

            }
        }

        impl ImportedThunks {
            pub fn new<M: cranelift_module::Module>(
                thunks: &Thunks,
                function: &mut cranelift_codegen::ir::function::Function,
                module: &mut M,
            ) -> Self {
                $(
                    let $name = module.declare_func_in_func(thunks.$name, function);
                )*

                Self {
                    $($name),*
                }
            }
        }

        impl<'gc, 'a, 'f> SSABuilder<'gc, 'a, 'f> {
            thunks! {
                @generate_ssa $gl, 'gc, 'a, 'f;
                $(
                    fn $name ($($arg: $t),*) -> $ret;
                )*
        }
        }
    };

    (@generate_ssa $l: lifetime, $gc: lifetime, $a: lifetime, $f: lifetime;
        $(fn $name: ident ($($arg: ident : $t: ty),*) -> $ret: ty;)*
    ) => {$(
        paste::paste! {
            pub fn [<emit_ $name>](&mut self, $($arg: impl IntoSSA<$gc, $a, $f>),*) -> cranelift_codegen::ir::entities::Inst {
                $(
                    let $arg = $arg.into_ssa(self);
                )*
                let ctx = self.builder.ins().get_pinned_reg(cranelift_codegen::ir::types::I64);
                self.builder.ins().call(self.thunks.$name, &[ctx, $($arg),*])
            }
        })*
    };

}

pub mod compiler {
    use cranelift_codegen::ir;
    use rsgc::{Gc, mmtk::util::Address};

    use crate::runtime::{
        value::{TypeCode8, TypeCode16, Value},
        vm::thunks::ThunkResult,
    };

    pub trait PrimType {
        fn clif_type() -> impl Iterator<Item = ir::Type>;
    }

    impl PrimType for TypeCode8 {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I8)
        }
    }

    impl PrimType for TypeCode16 {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I16)
        }
    }

    impl PrimType for i64 {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }

    impl PrimType for Value<'_> {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }

    impl PrimType for () {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::empty()
        }
    }

    impl PrimType for bool {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I8)
        }
    }

    impl PrimType for u8 {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I8)
        }
    }

    impl PrimType for u16 {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I16)
        }
    }

    impl PrimType for ThunkResult<'_> {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64).chain(std::iter::once(ir::types::I64))
        }
    }

    impl<T> PrimType for *mut T {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }

    impl<T> PrimType for *const T {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }

    impl PrimType for Address {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }

    impl<T> PrimType for &T {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }

    impl<'gc, T> PrimType for Gc<'gc, T> {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }

    impl PrimType for usize {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }
}

thunks! {
    'gc:
    pub fn wrong_number_of_args(
        ctx: &Context<'gc>,
        subr: Value<'gc>
    ) -> Value<'gc> {
        println!("wrong number of args: {}", subr);
        crate::runtime::vm::debug::print_stacktraces_impl(*ctx);
        todo!()
    }

    pub fn cons_rest(
        ctx: &Context<'gc>,
        rands: *mut Value<'gc>,
        num_rands: usize,
        from: usize
    ) -> Value<'gc> {
         let args = unsafe { std::slice::from_raw_parts(rands, num_rands) };

        let mut ls = Value::null();
        for &arg in args[from..].iter().rev() {
            ls = Value::cons(*ctx, arg, ls);
        }

        ls
    }

    pub fn non_applicable(
        ctx: &Context<'gc>,
        subr: Value<'gc>
    ) -> Value<'gc> {

        crate::runtime::vm::debug::print_stacktraces_impl(*ctx);
         println!("non applicable: {}", subr);
        todo!()
    }

    pub fn cons(
        ctx: &Context<'gc>,
        car: Value<'gc>,
        cdr: Value<'gc>
    ) -> Value<'gc> {
        Value::cons(*ctx, car, cdr)
    }

    pub fn make_box(
        ctx: &Context<'gc>,
        value: Value<'gc>
    ) -> Value<'gc> {

        Boxed::new(*ctx, value).into()
    }

    pub  fn lookup_bound(
        ctx: &Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>
    ) -> ThunkResult<'gc> {
        let variable = module.downcast::<Module>().variable(*ctx, name);

        let Some(variable) = variable else {
            println!("unbound variable: {}", name);
            return ThunkResult {
                code: 1,
                value: Value::new(false),
            };
        };

        if variable.get() == Value::undefined() {
            println!("unbound variable: {}", name);
            return ThunkResult {
                code: 1,
                value: Value::new(false),
            };
        }

        return ThunkResult {
            code: 0,
            value: variable.into(),
        };
    }

    pub fn lookup(
        ctx: &Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>
    ) -> ThunkResult<'gc> {
        let variable = module.downcast::<Module>().variable(*ctx, name);

        let Some(var) = variable else {
            println!("unbound variable: {}", name);
            return ThunkResult {
                code: 1,
                value: Value::new(false),
            };
        };

        return ThunkResult {
            code: 0,
            value: var.into(),
        };
    }

    pub fn define(
        ctx: &Context<'gc>,
        name: Value<'gc>,
        value: Value<'gc>
    ) -> Value<'gc> {
        let module = crate::runtime::modules::current_module(*ctx).get(*ctx).downcast::<Module>();

        module.define(*ctx, name, value);

        Value::undefined()
    }

    pub fn current_module(ctx: &Context<'gc>) -> Value<'gc> {
        crate::runtime::modules::current_module(*ctx).get(*ctx)
    }

    pub fn set_current_module(ctx: &Context<'gc>, module: Value<'gc>) -> Value<'gc> {
        crate::runtime::modules::set_current_module(*ctx, module);
        Value::undefined()
    }

    pub fn make_closure(
        ctx: &Context<'gc>,
        func: *const u8,
        nfree: usize,
        is_cont: bool,
        meta: Value<'gc>
    ) -> Value<'gc> {
  //      NCLOSURES.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let free = if nfree == 0 {
            Value::new(false)
        } else {
            Vector::new::<false>(&ctx, nfree, Value::undefined()).into()
        };
//        println!("make-closure {nfree}, code={func:p}, cont={}, meta={}", is_cont, meta);

        let clos = Closure {
            header: ScmHeader::with_type_bits(if is_cont {
                TypeCode16::CLOSURE_K.bits()
            } else {
                TypeCode16::CLOSURE_PROC.bits()
            }),
            code: Address::from_ptr(func),
            free,
            meta,
        };

        Gc::new(&ctx, clos).into()
    }

    pub fn fasl_read_nofail(
        ctx: &Context<'gc>,
        data: *const u8,
        size: usize
    ) -> Value<'gc> {
        let data = unsafe { std::slice::from_raw_parts(data, size) };

        let fasl = FASLReader::new(*ctx, data);

        let res = fasl.read().expect("failed to read FASL");

        res
    }

    pub fn yieldpoint(
        ctx: &Context<'gc>,
        rator: Value<'gc>,
        rands: *const Value<'gc>,
        num_rands: usize
    ) -> Gc<'gc, SavedCall<'gc>> {

        let args = unsafe { std::slice::from_raw_parts(rands, num_rands) };

        let arr = Array::from_slice(&ctx, args);

        Gc::new(&ctx, SavedCall { rands: arr, rator })
    }

    pub fn alloc_tc8(
        ctx: &Context<'gc>,
        vt: &'static VTable,
        tc8: TypeCode8,
        size: usize
    ) -> Value<'gc> {
        unsafe {
            let layout = Layout::from_size_align_unchecked(size, 8);
            let val = ctx
                .mc
                .raw_allocate(size, 8, vt, AllocationSemantics::Default);
            val.to_address()
                .store(ScmHeader::with_type_bits(tc8.bits() as _));

            Value::from_raw(val.to_address().as_usize() as u64)
        }
    }

    pub fn alloc_tc16(
        ctx: &Context<'gc>,
        vt: &'static VTable,
        tc16: TypeCode16,
        size: usize
    ) -> Value<'gc> {
        unsafe {
            let layout = Layout::from_size_align_unchecked(size, 8);
            let val = ctx
                .mc
                .raw_allocate(size, 8, vt, AllocationSemantics::Default);
            val.to_address()
                .store(ScmHeader::with_type_bits(tc16.bits() as _));

            Value::from_raw(val.to_address().as_usize() as u64)
        }
    }

    pub fn reverse(ctx: &Context<'gc>, list: Value<'gc>) -> Value<'gc> {
        list.list_reverse(*ctx)
    }

    pub fn append(
        ctx: &Context<'gc>,
        list1: Value<'gc>,
        list2: Value<'gc>
    ) -> Value<'gc> {
        list1.list_append(*ctx, std::iter::once(list2))
    }

    pub fn eqv(a: Value<'gc>, b: Value<'gc>) -> bool {
        a.eqv(b)
    }

    pub fn equal(a: Value<'gc>, b: Value<'gc>) -> bool {
        a.r5rs_equal(b)
    }

    pub fn plus(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        println!("plus {a} {b}");
        let Some(a) = a.number() else {
            println!("not a number {a}");
            todo!()
        };

        let Some(b) = b.number() else {
            println!("not a number {b}");
            todo!()
        };

        ThunkResult { code: 0, value: Number::add(*ctx, a, b).into_value(*ctx) }
    }

    pub fn minus(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            todo!()
        };

        let Some(b) = b.number() else {
            todo!()
        };

        ThunkResult { code: 0, value: Number::sub(*ctx, a, b).into_value(*ctx) }
    }

    pub fn times(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            todo!()
        };

        let Some(b) = b.number() else {
            todo!()
        };

        ThunkResult { code: 0, value: Number::mul(*ctx, a, b).into_value(*ctx) }
    }

    pub fn quotient(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            todo!()
        };

        let Some(b) = b.number() else {
            todo!()
        };

        ThunkResult { code: 0, value: Number::quotient(*ctx, a, b).into_value(*ctx) }
    }

    pub fn div(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            todo!()
        };

        let Some(b) = b.number() else {
            todo!()
        };

        ThunkResult { code: 0, value: Number::div(*ctx, a, b).into_value(*ctx) }
    }

    pub fn remainder(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            todo!()
        };

        let Some(b) = b.number() else {
            todo!()
        };

        ThunkResult { code: 0, value: Number::remainder(*ctx, a, b).into_value(*ctx) }
    }

    pub fn is_real(value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            return ThunkResult {
                code: 0,
                value: false.into()
            }
        };

        ThunkResult { code: 0, value: num.is_real().into() }
    }

    pub fn is_rational(value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            return ThunkResult {
                code: 0,
                value: false.into()
            }
        };

        ThunkResult { code: 0, value: num.is_rational().into() }
    }

    pub fn is_integer(value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            return ThunkResult {
                code: 0,
                value: false.into()
            }
        };

        ThunkResult { code: 0, value: num.is_integer().into() }
    }

    pub fn is_exact_integer(value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            return ThunkResult {
                code: 0,
                value: false.into()
            }
        };

        ThunkResult { code: 0, value: num.is_exact_integer().into() }
    }

    pub fn is_exact_nonnegative_integer(value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            return ThunkResult {
                code: 0,
                value: false.into()
            }
        };

        ThunkResult { code: 0, value: num.is_exact_non_negative_integer().into() }
    }

    pub fn is_exact_positive_integer(value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            return ThunkResult {
                code: 0,
                value: false.into()
            }
        };

        ThunkResult { code: 0, value: num.is_exact_positive_integer().into() }
    }

    pub fn is_inexact_real(value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            return ThunkResult {
                code: 0,
                value: false.into()
            }
        };
        todo!()
    }

    pub fn is_zero(ctx: &Context<'gc>, value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            todo!()
        };
        ThunkResult { code: 0, value: num.is_zero().into() }
    }

    pub fn is_less_than(ctx: &Context<'gc>, value: Value<'gc>, other: Value<'gc>) -> ThunkResult<'gc> {
        let Some(value) = value.number() else {
            todo!()
        };

        let Some(other) = other.number() else {
            todo!()
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, value, other) == Some(Ordering::Less)).into() }
    }

    pub fn is_less_than_equal(ctx: &Context<'gc>, value: Value<'gc>, other: Value<'gc>) -> ThunkResult<'gc> {
        let Some(value) = value.number() else {
            todo!()
        };

        let Some(other) = other.number() else {
            todo!()
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, value, other) != Some(Ordering::Greater)).into() }
    }

    pub fn is_greater_than(ctx: &Context<'gc>, value: Value<'gc>, other: Value<'gc>) -> ThunkResult<'gc> {
        let Some(value) = value.number() else {
            todo!()
        };

        let Some(other) = other.number() else {
            todo!()
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, value, other) == Some(Ordering::Greater)).into() }
    }

    pub fn is_greater_than_equal(ctx: &Context<'gc>, value: Value<'gc>, other: Value<'gc>) -> ThunkResult<'gc> {
        let Some(value) = value.number() else {
            todo!()
        };

        let Some(other) = other.number() else {
            todo!()
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, value, other) != Some(Ordering::Less)).into() }
    }

    pub fn is_numerically_equal(ctx: &Context<'gc>, value: Value<'gc>, other: Value<'gc>) -> ThunkResult<'gc> {
        let Some(value) = value.number() else {
            todo!()
        };

        let Some(other) = other.number() else {
            todo!()
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, value, other) == Some(Ordering::Equal)).into() }
    }

    pub fn negate(ctx: &Context<'gc>, value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            todo!()
        };

        ThunkResult { code: 0, value: num.negate(*ctx).into_value(*ctx) }
    }

    pub fn debug_trace(ctx: &Context<'gc>, rator: Value<'gc>, rands: *const Value<'gc>, num_rands: usize) -> () {
        let ip = unsafe { returnaddress(0) };
        let rands = unsafe { std::slice::from_raw_parts(rands, num_rands).to_vec() };
        let frame = debug::ShadowFrame { ip: ip as u64, rator, rands, meta: Value::new(false) };
        unsafe {
            (*ctx.state.shadow_stack.get()).push(frame);
        }
    }

}

#[unsafe(no_mangle)]
pub static BOX_VTABLE: &'static VTable = &VTableOf::<Boxed>::VT;
#[unsafe(no_mangle)]
pub static PAIR_VTABLE: &'static VTable = &VTableOf::<Pair>::VT;
#[unsafe(no_mangle)]
pub static VECTOR_VTABLE: &'static VTable = &Vector::VT;
#[unsafe(no_mangle)]
pub static TUPLE_VTABLE: &'static VTable = &Tuple::VT;

unsafe extern "C" {
    #[link_name = "llvm.returnaddress"]
    fn returnaddress(_: i32) -> *const u8;
}

pub fn make_assertion_violation<'gc>(
    ctx: &Context<'gc>,
    who: Value<'gc>,
    message: Value<'gc>,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let args = std::iter::once(who)
        .chain(std::iter::once(message))
        .chain(irritants.iter().cloned())
        .collect::<Vec<_>>();

    let assertion_violation = root_module(*ctx)
        .get(
            *ctx,
            Symbol::from_str(*ctx, ".make-assertion-violation").into(),
        )
        .expect("pre boot code");

    match call_scheme(*ctx, assertion_violation, args) {
        VMResult::Ok(val) => val,
        VMResult::Err(err) => err,
        VMResult::Yield => unreachable!(),
    }
}

pub fn make_error<'gc>(
    ctx: &Context<'gc>,
    who: Value<'gc>,
    message: Value<'gc>,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let args = std::iter::once(who)
        .chain(std::iter::once(message))
        .chain(irritants.iter().cloned())
        .collect::<Vec<_>>();

    let error = root_module(*ctx)
        .get(*ctx, Symbol::from_str(*ctx, ".make-error").into())
        .expect("pre boot code");

    match call_scheme(*ctx, error, args) {
        VMResult::Ok(val) => val,
        VMResult::Err(err) => err,
        VMResult::Yield => unreachable!(),
    }
}

pub fn make_io_error<'gc>(
    ctx: &Context<'gc>,
    who: &str,
    message: Value<'gc>,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let who = Symbol::from_str(*ctx, who).into();
    let args = std::iter::once(who)
        .chain(std::iter::once(message))
        .chain(irritants.iter().cloned())
        .collect::<Vec<_>>();

    let io_error = root_module(*ctx)
        .get(*ctx, Symbol::from_str(*ctx, ".make-io-error").into())
        .unwrap_or_else(|| {
            panic!(
                "pre boot code, who={who}, message={message}, irritants={:?}",
                irritants
            )
        });

    match call_scheme(*ctx, io_error, args) {
        VMResult::Ok(val) => val,
        VMResult::Err(err) => err,
        VMResult::Yield => unreachable!(),
    }
}

pub fn make_lexical_violation<'gc>(
    ctx: &Context<'gc>,
    who: &str,
    message: impl AsRef<str>,
) -> Value<'gc> {
    let who = Symbol::from_str(*ctx, who).into();
    let message: Value = Str::new(&ctx, message, true).into();
    let args = vec![who, message];

    let lexical_violation = root_module(*ctx)
        .get(
            *ctx,
            Symbol::from_str(*ctx, ".make-lexical-violation").into(),
        )
        .unwrap_or_else(|| panic!("pre boot code, who={who}, message={message}",));

    match call_scheme(*ctx, lexical_violation, args) {
        VMResult::Ok(val) => val,
        VMResult::Err(err) => err,
        VMResult::Yield => unreachable!(),
    }
}
