//! VM thunks -- functions directly callable from compiled code.
//!
//!
//! This file largely copies bunch of functions from other places
//! in order to make them callable from compiled code.

#![allow(dead_code, unused_variables)]
use crate::runtime::{
    Context,
    fasl::FASLReader,
    modules::{Module, Variable, root_module},
    value::{
        Boxed, ByteVector, Closure, Complex, Pair, SavedCall, ScmHeader, Str, Symbol, Tuple,
        TypeCode8, TypeCode16, Value, Vector,
    },
    vm::{
        VMResult, call_scheme,
        debug::{self, print_stacktraces_impl},
    },
};
use crate::{
    compiler::ssa::{SSABuilder, traits::IntoSSA},
    runtime::value::{IntoValue, Number},
};
use cranelift_codegen::ir::InstBuilder;
use rsgc::{
    Gc, ObjectSlot,
    alloc::Array,
    mmtk::{
        AllocationSemantics, MutatorContext,
        util::{Address, ObjectReference},
    },
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
            #[unsafe(export_name=concat!("capy_thunks_", stringify!($name)))]
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

                    let $name = module.declare_function(concat!("capy_thunks_", stringify!($name)), cranelift_module::Linkage::Import, &sig).unwrap();
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
    use rsgc::{
        Gc, ObjectSlot,
        mmtk::util::{Address, ObjectReference},
    };

    use crate::runtime::{
        value::{TypeCode8, TypeCode16, Value},
        vm::thunks::ThunkResult,
    };

    pub trait PrimType {
        fn clif_type() -> impl Iterator<Item = ir::Type>;
    }

    impl PrimType for ObjectSlot {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
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

    impl PrimType for isize {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }

    impl PrimType for i32 {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I32)
        }
    }

    impl PrimType for ObjectReference {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }
}

thunks! {
    'gc:

    pub fn wrong_number_of_args(
        ctx: &Context<'gc>,
        subr: Value<'gc>,
        got: usize,
        expected: isize
    ) -> Value<'gc> {
        //print_stacktraces_impl(*ctx);
        let is_cont = subr.is::<Closure>() && subr.downcast::<Closure>().is_continuation();
        let msg = if is_cont {
            let ret = unsafe { returnaddress(0) };
            backtrace::resolve(ret as _, |sym| {
                println!("{sym:?}");
            });
            if expected < 0 {
                format!("expected at least {} values, got {}", -expected, got)
            } else {
                format!("expected {} value(s), got {}", expected, got)
            }
        } else {
            crate::runtime::vm::debug::print_stacktraces_impl(*ctx);
            if expected < 0 {
                format!("procedure expected at least {} arguments, got {}", -expected, got)
            } else {
                format!("procedure expected {} arguments, got {}", expected, got)
            }
        };

        let meta = if subr.is::<Closure>() {
            subr.downcast::<Closure>().meta.get()
        } else {
            Value::new(false)
        };
        make_assertion_violation(
            ctx,
            Value::new(false),
            Str::new(&ctx, &msg, true).into(),
            &[subr, meta]
        )
    }

    pub fn cons_rest(
        ctx: &Context<'gc>,
        rands: *mut Value<'gc>,
        num_rands: usize,
        from: usize
    ) -> Value<'gc> {
         let args = unsafe { std::slice::from_raw_parts(rands, num_rands) };

        let mut ls = Value::null();
        if from >= args.len() {
        }
        for &arg in args[from..].iter().rev() {
            ls = Value::cons(*ctx, arg, ls);
        }

        ls
    }

    pub fn non_applicable(
        ctx: &Context<'gc>,
        subr: Value<'gc>
    ) -> Value<'gc> {
        println!("call {subr}");
        crate::runtime::vm::debug::print_stacktraces_impl(*ctx);
        let ret = unsafe { returnaddress(0) };
        backtrace::resolve(ret as _, |sym| {
            println!("{sym:?}");
        });
        make_assertion_violation(
            ctx,
            Value::new(false),
            Str::new(&ctx, "attempt to call non-procedure", true).into(),
            &[subr]
        )

    }

    pub fn make_variable(
        ctx: &Context<'gc>,
        value: Value<'gc>
    ) -> Value<'gc> {


        Variable::new(*ctx, value).into()
    }

    pub fn lookup_bound(
        ctx: &Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>
    ) -> ThunkResult<'gc> {
        assert!(name.is::<Symbol>());
        if !module.is::<Module>() {
            unreachable!("lookup-bound: not a module: {}", module);
        }
        let variable = module.downcast::<Module>().variable(*ctx, name);

        let Some(variable) = variable else {
            let ret = unsafe { returnaddress(0) };
            backtrace::resolve(ret as _, |sym| {
                println!("{sym:?}");
            });
            return ThunkResult {
                code: 1,
                value:
                    make_undefined_violation(ctx, name, &format!("variable not found in module '{}'", module.downcast::<Module>().name.get()), &[name, module]),
            };
        };

        if variable.get() == Value::undefined() {

            return ThunkResult {
                code: 1,
                value: make_undefined_violation(ctx, name, &format!("variable not bound in module '{}'", module.downcast::<Module>().name.get()), &[name, module]),
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
            let ret = unsafe { returnaddress(0) };
            backtrace::resolve(ret as _, |sym| {
                println!("{sym:?}");
            });
            return ThunkResult {
                code: 1,
                value: make_undefined_violation(ctx, name, &format!("variable not found in module '{}'", module.downcast::<Module>().name.get()), &[name, module]),
            };
        };

        return ThunkResult {
            code: 0,
            value: var.into(),
        };
    }

    pub fn lookup_bound_public(
        ctx: &Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>
    ) -> ThunkResult<'gc> {
        let module = resolve_module(*ctx, module, true);
        if module.code != 0 {
            return module;
        }

        let var = lookup(ctx, module.value, name);
        if var.code != 0 {
            return var;
        }

        let var = var.value.downcast::<Variable>();
        if !var.is_bound() {

            return ThunkResult {
                code: 1,
                value: make_undefined_violation(ctx, name, &format!("variable not bound in module '{}'", module.value.downcast::<Module>().name.get()), &[name, module.value]),
            };
        }

        ThunkResult { code: 0, value: var.into() }
    }

    pub fn lookup_bound_private(
        ctx: &Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>
    ) -> ThunkResult<'gc> {
        let module = resolve_module(*ctx, module, false);
        if module.code != 0 {
            return module;
        }

        let var = lookup(ctx, module.value, name);
        if var.code != 0 {
            return var;
        }

        let var = var.value.downcast::<Variable>();
        if !var.is_bound() {
            return ThunkResult {
                code: 1,
                value: make_undefined_violation(ctx, name, &format!("variable not bound in module '{}'", module.value.downcast::<Module>().name.get()), &[name, module.value]),
            };
        }

        ThunkResult { code: 0, value: var.into() }
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
        let module = crate::runtime::modules::current_module(*ctx).get(*ctx);
        if !module.is::<Module>() {

            print_stacktraces_impl(*ctx);
            panic!("current-module: not a module: {}", module);
        }
        module
    }

    pub fn set_current_module(ctx: &Context<'gc>, module: Value<'gc>) -> Value<'gc> {
        if !module.is::<Module>() {
            let ret = unsafe { returnaddress(0) };
            backtrace::resolve(ret as _, |sym| {
                println!("{sym:?}");
            });
            println!("set-current-module: not a module: {}", module);
            print_stacktraces_impl(*ctx);
            panic!("set-current-module: not a module: {}", module);
        }
        crate::runtime::modules::set_current_module(*ctx, module);
        Value::undefined()
    }

    pub fn module_ensure_local_variable(ctx: &Context<'gc>, module: Value<'gc>, name: Value<'gc>) -> Value<'gc> {
        let module = module.downcast::<Module>();
        let variable = module.ensure_local_variable(*ctx, name);
        variable.into()
    }

    pub fn make_closure(
        ctx: &Context<'gc>,
        func: *const u8,
        nfree: usize,
        is_cont: bool,
        meta: Value<'gc>
    ) -> Value<'gc> {

        let free = if nfree == 0 {
            Value::new(false)
        } else {
            Vector::new::<false>(&ctx, nfree, Value::undefined()).into()
        };

        let meta = if meta == Value::new(false) {
            Value::null()
        } else {
            meta
        };

        let clos = Closure {
            header: ScmHeader::with_type_bits(if is_cont {
                TypeCode16::CLOSURE_K.bits()
            } else {
                TypeCode16::CLOSURE_PROC.bits()
            }),
            code: Address::from_ptr(func),
            direct: Address::ZERO,
            free,
            meta: rsgc::cell::Lock::new(meta),
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
        ctx.state.runstack.set(ctx.state.runstack_start);
        let args = unsafe { std::slice::from_raw_parts(rands, num_rands) };


        let arr = Array::from_slice(&ctx, args);

        Gc::new(&ctx, SavedCall { rands: arr, rator, from_procedure: true })
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

    pub fn eqv(a: Value<'gc>, b: Value<'gc>) -> bool {
        a.eqv(b)
    }

    pub fn equal(a: Value<'gc>, b: Value<'gc>) -> bool {
        a.equal(b, &mut Default::default())
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

    pub fn is_rational(value: Value<'gc>) -> bool {
        let Some(num) = value.number() else {
            return false;
        };

        num.is_rational()
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
        ThunkResult { code: 0, value: num.is_flonum().into() }
    }

    pub fn is_zero(ctx: &Context<'gc>, value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "zero?").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[value],
                )
            }
        };
        ThunkResult { code: 0, value: num.is_zero().into() }
    }

    pub fn is_less_than(ctx: &Context<'gc>, value: Value<'gc>, other: Value<'gc>) -> ThunkResult<'gc> {
        let Some(value) = value.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "<").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[value],
                )
            }
        };

        let Some(other) = other.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "<").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[other],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, value, other) == Some(Ordering::Less)).into() }
    }

    pub fn is_less_than_equal(ctx: &Context<'gc>, value: Value<'gc>, other: Value<'gc>) -> ThunkResult<'gc> {
        let Some(value) = value.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "<=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[value],
                )
            }
        };

        let Some(other) = other.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "<=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[other],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, value, other) != Some(Ordering::Greater)).into() }
    }

    pub fn is_greater_than(ctx: &Context<'gc>, value: Value<'gc>, other: Value<'gc>) -> ThunkResult<'gc> {
        let Some(value) = value.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, ">").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[value],
                )
            }
        };

        let Some(other) = other.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, ">").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[other],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, value, other) == Some(Ordering::Greater)).into() }
    }

    pub fn is_greater_than_equal(ctx: &Context<'gc>, value: Value<'gc>, other: Value<'gc>) -> ThunkResult<'gc> {
        let Some(value) = value.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, ">=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[value],
                )
            }
        };

        let Some(other) = other.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, ">=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[other],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, value, other) != Some(Ordering::Less)).into() }
    }

    pub fn is_numerically_equal(ctx: &Context<'gc>, value: Value<'gc>, other: Value<'gc>) -> ThunkResult<'gc> {
        let Some(value) = value.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[value],
                )
            }
        };

        let Some(other) = other.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[other],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, value, other) == Some(Ordering::Equal)).into() }
    }

    pub fn negate(ctx: &Context<'gc>, value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(num) = value.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "negate").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[value],
                )
            }
        };

        ThunkResult { code: 0, value: num.negate(*ctx).into_value(*ctx) }
    }

    pub fn debug_trace(ctx: &Context<'gc>, rator: Value<'gc>, rands: *const Value<'gc>, num_rands: usize, meta: Value<'gc>) -> () {
        let ip = unsafe { returnaddress(0) };
        let rands = unsafe { std::slice::from_raw_parts(rands, num_rands).to_vec() };

        unsafe {
            let frame = debug::ShadowFrame { ip: ip as u64, rator, rands, meta };
            (*ctx.state.shadow_stack.get()).push(frame);
        }
    }

    pub fn memv(ctx: &Context<'gc>, key: Value<'gc>, list: Value<'gc>) -> ThunkResult<'gc> {
        if !list.is_list() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "memv").into(),
                    Str::new(ctx, "not a proper list", true).into(),
                    &[list],
                )
            }
        }

        let mut cur = list;
        while !cur.is_null() {

            if key.eqv(cur.car()) {
                return ThunkResult { code: 0, value: cur };
            }
            cur = cur.cdr();
        }

        ThunkResult { code: 0, value: Value::new(false) }
    }

    pub fn memq(ctx: &Context<'gc>, key: Value<'gc>, list: Value<'gc>) -> ThunkResult<'gc> {
        if !list.is_list() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "memq").into(),
                    Str::new(ctx, "not a proper list", true).into(),
                    &[list],
                )
            }
        }

        let mut cur = list;
        while !cur.is_null() {
            if key == cur.car() {
                return ThunkResult { code: 0, value: cur };
            }
            cur = cur.cdr();
        }

        ThunkResult { code: 0, value: Value::new(false) }
    }

    pub fn number_eq(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, a, b) == Some(Ordering::Equal)).into() }
    }

    pub fn number_lt(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "<").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "<").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, a, b) == Some(Ordering::Less)).into() }
    }

    pub fn number_gt(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, ">").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, ">").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, a, b) == Some(Ordering::Greater)).into() }
    }

    pub fn number_le(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "<=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "<=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, a, b) != Some(Ordering::Greater)).into() }
    }

    pub fn number_ge(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, ">=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, ">=").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: (Number::compare(*ctx, a, b) != Some(Ordering::Less)).into() }
    }

    pub fn string2symbol(ctx: &Context<'gc>, s: Value<'gc>) -> ThunkResult<'gc> {
        let Some(s) = s.try_as::<Str>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string->symbol").into(),
                    Str::new(ctx, "not a string", true).into(),
                    &[s],
                )
            }
        };

        ThunkResult { code: 0, value: Symbol::from_string(*ctx, s).into() }
    }

    pub fn symbol2string(ctx: &Context<'gc>, s: Value<'gc>) -> ThunkResult<'gc> {
        let Some(s) = s.try_as::<Symbol>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "symbol->string").into(),
                    Str::new(ctx, "not a symbol", true).into(),
                    &[s],
                )
            }
        };


        ThunkResult { code: 0, value: s.to_str(ctx).into() }
    }

    pub fn number_plus(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "+").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "+").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: Number::add(*ctx, a, b).into_value(*ctx) }
    }

    pub fn number_minus(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "-").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "-").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: Number::sub(*ctx, a, b).into_value(*ctx) }
    }

    pub fn number_times(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "*").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "*").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: Number::mul(*ctx, a, b).into_value(*ctx) }
    }

    pub fn number_div(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "/").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "/").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if b.is_zero() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "/").into(),
                    Str::new(ctx, "division by zero", true).into(),
                    &[a.into_value(*ctx), b.into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: Number::div(*ctx, a, b).into_value(*ctx) }
    }

    pub fn quotient(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "quotient").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "quotient").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if b.is_zero() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "quotient").into(),
                    Str::new(ctx, "division by zero", true).into(),
                    &[a.into_value(*ctx), b.into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: Number::quotient(*ctx, a, b).into_value(*ctx) }
    }

    pub fn remainder(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "remainder").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "remainder").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if b.is_zero() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "remainder").into(),
                    Str::new(ctx, "division by zero", true).into(),
                    &[a.into_value(*ctx), b.into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: Number::remainder(*ctx, a, b).into_value(*ctx) }
    }

    pub fn modulo(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "modulo").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "modulo").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if b.is_zero() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "modulo").into(),
                    Str::new(ctx, "division by zero", true).into(),
                    &[a.into_value(*ctx), b.into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: Number::modulo(*ctx, a, b).into_value(*ctx) }
    }

    pub fn exact2inexact(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "exact->inexact").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };


        ThunkResult { code: 0, value: n.to_inexact(*ctx).into_value(*ctx) }
    }

    pub fn inexact_to_exact(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "inexact->exact").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };
        ThunkResult { code: 0, value: n.to_exact(*ctx).into_value(*ctx) }
    }

    pub fn expt(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "expt").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "expt").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        ThunkResult { code: 0, value: Number::expt(*ctx, a, b).into_value(*ctx) }
    }

    pub fn ash(ctx: &Context<'gc>, n: Value<'gc>, count: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "ash").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        let Some(count) = count.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "ash").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[count],
                )
            }
        };

        if !count.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "ash").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[count.into_value(*ctx)],
                )
            }
        }

        if !n.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "ash").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[n.into_value(*ctx)],
                )
            }
        }

        if !count.is_negative() {

            let Some(res) = n.lsh(*ctx, count) else {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(*ctx, "ash").into(),
                        Str::new(ctx, "shift out of bounds", true).into(),
                        &[n.into_value(*ctx), count.into_value(*ctx)],
                    )
                }
            };

            ThunkResult { code: 0, value: res.into_value(*ctx) }
        } else {
            let count = count.negate(*ctx);
            let Some(res) = n.rsh(*ctx, count) else {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(*ctx, "ash").into(),
                        Str::new(ctx, "shift out of bounds", true).into(),
                        &[n.into_value(*ctx), count.into_value(*ctx)],
                    )
                };
            };

            ThunkResult { code: 0, value: res.into_value(*ctx) }
        }
    }


    pub fn logand(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            print_stacktraces_impl(*ctx);
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logand").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
             print_stacktraces_impl(*ctx);
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logand").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if !a.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logand").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[a.into_value(*ctx)],
                )
            }
        }

        if !b.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logand").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[b.into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: a.logand(*ctx, b).into_value(*ctx) }
    }

    pub fn logior(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logior").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logior").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if !a.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logior").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[a.into_value(*ctx)],
                )
            }
        }

        if !b.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logior").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[b.into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: a.logior(*ctx, b).into_value(*ctx) }
    }

    pub fn logxor(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logxor").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logxor").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if !a.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logxor").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[a.into_value(*ctx)],
                )
            }
        }

        if !b.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logxor").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[b.into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: a.logxor(*ctx, b ).into_value(*ctx) }
    }

    pub fn lognot(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "lognot").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        if !n.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "lognot").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[n.into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: n.lognot(*ctx).into_value(*ctx) }
    }

    pub fn logtest(ctx: &Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> ThunkResult<'gc> {
        let Some(a) = a.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logtest").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[a],
                )
            }
        };

        let Some(b) = b.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logtest").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[b],
                )
            }
        };

        if !a.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logtest").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[a.into_value(*ctx)],
                )
            }
        }

        if !b.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logtest").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[b.into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: (!a.logand(*ctx, b).is_zero()).into() }
    }

    pub fn logbitp(ctx: &Context<'gc>, k: Value<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(k) = k.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logbitp").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[k],
                )
            }
        };

        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logbitp").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        if !k.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logbitp").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[k.into_value(*ctx)],
                )
            }
        }

        if !n.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "logbitp").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[n.into_value(*ctx)],
                )
            }
        }


        ThunkResult { code: 0, value: k.logbit(*ctx, n).into() }
    }

    pub fn sqrt(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "sqrt").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::sqrt(*ctx, n).into_value(*ctx) }
    }

    pub fn abs(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "abs").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: n.abs(*ctx).into_value(*ctx) }
    }

    pub fn floor(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "floor").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: n.floor(*ctx).into_value(*ctx) }
    }

    pub fn ceiling(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "ceiling").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: n.ceiling(*ctx).into_value(*ctx) }
    }

    pub fn truncate(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "truncate").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: n.truncate(*ctx).into_value(*ctx) }
    }

    pub fn sin(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "sin").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::sin(*ctx, n).into_value(*ctx) }
    }

    pub fn cos(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "cos").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::cos(*ctx, n).into_value(*ctx) }
    }

    pub fn tan(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tan").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::tan(*ctx, n).into_value(*ctx) }
    }

    pub fn asin(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "asin").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::asin(*ctx, n).into_value(*ctx) }
    }

    pub fn acos(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "acos").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        ThunkResult { code: 0, value: Number::acos(*ctx, n).into_value(*ctx) }
    }

    pub fn atan(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "atan").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        todo!("atan of {n}")
    }

    pub fn not(v: Value<'gc>) -> bool {
        v == Value::new(false)
    }

    pub fn pairp(v: Value<'gc>) -> bool {
        v.is::<Pair>()
    }

    pub fn nullp(v: Value<'gc>) -> bool {
        v.is_null()
    }

    pub fn listp(v: Value<'gc>) -> bool {
        v.is_list()
    }

    pub fn symbolp( v: Value<'gc>) -> bool {
        v.is::<Symbol>()
    }

    pub fn vectorp(v: Value<'gc>) -> bool {
        v.is::<Vector>()
    }

    pub fn stringp(v: Value<'gc>) -> bool {
        v.is::<Str>()
    }

    pub fn numberp(v: Value<'gc>) -> bool {
        v.is_number()
    }

    pub fn charp( v: Value<'gc>) -> bool {
        v.is_char()
    }

    pub fn eof_objectp( v: Value<'gc>) -> bool {
        v.bits() == Value::VALUE_EOF as u64
    }

    pub fn tuplep(v: Value<'gc>) -> bool {
        v.is::<Tuple>()
    }

    pub fn bytevectorp(v: Value<'gc>) -> bool {
        v.is::<ByteVector>()
    }

    pub fn procedurep(v: Value<'gc>) -> bool {
        v.is::<Closure>()
    }

    pub fn complexp(v: Value<'gc>) -> bool {
        v.is_number()
    }

    pub fn realp(v: Value<'gc>) -> bool {
        v.is_number() && !v.is::<Complex>()
    }

    pub fn infp(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = v.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "infinite?").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[v],
                )
            }
        };

        ThunkResult { code: 0, value: (!n.is_finite()).into() }
    }

    pub fn nanp(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = v.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "nan?").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[v],
                )
            }
        };

        ThunkResult { code: 0, value: n.is_nan().into() }
    }

    pub fn integerp(ctx: &Context<'gc>, v: Value<'gc>) -> bool {
        v.number().map_or(false, |n| n.is_integer())
    }

    pub fn exactp(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = v.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "exact?").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[v],
                )
            }
        };

        ThunkResult { code: 0, value: n.is_exact().into() }
    }

    pub fn inexactp(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = v.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "inexact?").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[v],
                )
            }
        };

        ThunkResult { code: 0, value: (!n.is_exact()).into() }
    }

    pub fn evenp(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = v.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "even?").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[v],
                )
            }
        };



        ThunkResult { code: 0, value: n.is_even().into() }
    }

    pub fn oddp(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = v.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "odd?").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[v],
                )
            }
        };

        ThunkResult { code: 0, value: (!n.is_even()).into() }
    }

    pub fn exact_integerp(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = v.number() else {
            return ThunkResult {
                code: 0,
                value: Value::new(false)
            }
        };

        ThunkResult { code: 0, value: (n.is_exact_integer()).into() }
    }

    pub fn char_to_integer(ctx: &Context<'gc>, c: Value<'gc>) -> ThunkResult<'gc> {

        if !c.is_char() {
            let ret = unsafe { returnaddress(0) };
            backtrace::resolve(ret as *mut _, |symbol| {
                println!("{symbol:?}");
            });
            crate::runtime::vm::debug::print_stacktraces_impl(*ctx);
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "char->integer").into(),
                    Str::new(ctx, "not a char", true).into(),
                    &[c],
                )
            }
        }
        let c = c.char();


        ThunkResult { code: 0, value: Number::from_u32(*ctx, c as u32).into_value(*ctx) }
    }

    pub fn integer_to_char(ctx: &Context<'gc>, n: Value<'gc>) -> ThunkResult<'gc> {
        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "integer->char").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        if !n.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "integer->char").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[n.into_value(*ctx)],
                )
            }
        }

        let Some(u) = n.exact_integer_to_u32() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "integer->char").into(),
                    Str::new(ctx, "not in char range", true).into(),
                    &[n.into_value(*ctx)],
                )
            }
        };

        let Some(c) = std::char::from_u32(u) else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "integer->char").into(),
                    Str::new(ctx, "not in char range", true).into(),
                    &[n.into_value(*ctx)],
                )
            }
        };

        ThunkResult { code: 0, value: Value::from_char(c) }
    }

    pub fn number2string(ctx: &Context<'gc>, n: Value<'gc>, radix: Value<'gc>) -> ThunkResult<'gc> {

        let radix = if radix == Value::undefined() {
            10
        } else {
            let Some(radix) = radix.number() else {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(*ctx, "number->string").into(),
                        Str::new(ctx, "not a number", true).into(),
                        &[radix],
                    )
                }
            };

            if !radix.is_exact_integer() {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(*ctx, "number->string").into(),
                        Str::new(ctx, "not an exact integer", true).into(),
                        &[radix.into_value(*ctx)],
                    )
                }
            }

            let Some(radix) = radix.exact_integer_to_i32() else {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(*ctx, "number->string").into(),
                        Str::new(ctx, "not in i32 range", true).into(),
                        &[radix.into_value(*ctx)],
                    )
                }
            };

            if radix < 2 || radix > 36 {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(*ctx, "number->string").into(),
                        Str::new(ctx, "radix out of range", true).into(),
                        &[Number::Fixnum(radix).into_value(*ctx)],
                    )
                }
            }

            radix
        };

        let Some(n) = n.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "number->string").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[n],
                )
            }
        };

        let s = Str::new(ctx, n.to_string_radix(radix as _), false);

        ThunkResult { code: 0, value: s.into() }
    }

    pub fn string2number(ctx: &Context<'gc>, s: Value<'gc>, digits: Value<'gc>) -> ThunkResult<'gc> {
        let Some(s) = s.try_as::<Str>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string->number").into(),
                    Str::new(ctx, "not a string", true).into(),
                    &[s],
                )
            }
        };

        let radix = if digits == Value::undefined() {
            10
        } else {
            let Some(radix) = digits.number() else {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(*ctx, "string->number").into(),
                        Str::new(ctx, "not a number", true).into(),
                        &[digits],
                    )
                }
            };

            if !radix.is_exact_integer() {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(*ctx, "string->number").into(),
                        Str::new(ctx, "not an exact integer", true).into(),
                        &[radix.into_value(*ctx)],
                    )
                }
            }

            let Some(radix) = radix.exact_integer_to_i32() else {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(*ctx, "string->number").into(),
                        Str::new(ctx, "not in i32 range", true).into(),
                        &[radix.into_value(*ctx)],
                    )
                }
            };

            if radix < 2 || radix > 36 {
                return ThunkResult {
                    code: 1,
                    value: make_assertion_violation(ctx,
                        Symbol::from_str(*ctx, "string->number").into(),
                        Str::new(ctx, "radix out of range", true).into(),
                        &[Number::Fixnum(radix).into_value(*ctx)],
                    )
                }
            }

            radix
        };
        let s = if radix == 10 {
            s.to_string()
        } else if radix == 2 {
            format!("#b{s}")
        } else if radix == 8 {
            format!("#o{s}")
        } else if radix == 16 {
            format!("#x{s}")
        } else if radix != 10 {
            format!("#{radix}r{s}")
        } else {
            s.to_string()
        };



        let n = crate::frontend::num::parse_number(&s);

        match n {
            Ok(n) => ThunkResult { code: 0, value: n.to_vm_number(*ctx).into_value(*ctx) },
            Err(_) => return ThunkResult { code: 0, value: Value::new(false) }

        }
    }

    pub fn append(ctx: &Context<'gc>, m1: Value<'gc>, m2: Value<'gc>) -> ThunkResult<'gc> {
        if !m1.is_list() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "append").into(),
                    Str::new(ctx, "not a list", true).into(),
                    &[m1],
                )
            }
        }

        ThunkResult { code: 0, value: m1.append(*ctx, m2) }
    }

    pub fn car(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(p) = v.try_as::<Pair>() else {

            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "car").into(),
                    Str::new(ctx, "not a pair", true).into(),
                    &[v],
                )
            }
        };

        ThunkResult { code: 0, value: p.car() }
    }

    pub fn cdr(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(p) = v.try_as::<Pair>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "cdr").into(),
                    Str::new(ctx, "not a pair", true).into(),
                    &[v],
                )
            }
        };

        ThunkResult { code: 0, value: p.cdr() }
    }

    pub fn set_car(ctx: &Context<'gc>, v: Value<'gc>, new_car: Value<'gc>) -> ThunkResult<'gc> {
        let Some(p) = v.try_as::<Pair>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "set-car!").into(),
                    Str::new(ctx, "not a pair", true).into(),
                    &[v],
                )
            }
        };

        p.set_car(*ctx, new_car);

        ThunkResult { code: 0, value: Value::undefined() }
    }

    pub fn set_cdr(ctx: &Context<'gc>, v: Value<'gc>, new_cdr: Value<'gc>) -> ThunkResult<'gc> {
        let Some(p) = v.try_as::<Pair>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "set-cdr!").into(),
                    Str::new(ctx, "not a pair", true).into(),
                    &[v],
                )
            }
        };

        p.set_cdr(*ctx, new_cdr);

        ThunkResult { code: 0, value: Value::undefined() }
    }

    /* CXRs should be expanded already... */

    pub fn length(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        if !v.is_list() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "length").into(),
                    Str::new(ctx, "not a list", true).into(),
                    &[v],
                )
            }
        }

        ThunkResult { code: 0, value: Number::from_usize(*ctx, v.list_length()).into_value(*ctx) }
    }

    pub fn cons(ctx: &Context<'gc>, car: Value<'gc>, cdr: Value<'gc>) -> Value<'gc> {
        Value::cons(*ctx, car, cdr)
    }

    pub fn make_vector(ctx: &Context<'gc>, size: Value<'gc>, fill: Value<'gc>) -> ThunkResult<'gc> {
        let Some(size) = size.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "make-vector").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[size],
                )
            }
        };

        if !size.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "make-vector").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[size.into_value(*ctx)],
                )
            }
        }

        let Some(size) = size.exact_integer_to_usize() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "make-vector").into(),
                    Str::new(ctx, "not in usize range", true).into(),
                    &[size.into_value(*ctx)],
                )
            }
        };

        if size >= i32::MAX as usize {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "make-vector").into(),
                    Str::new(ctx, "size too large", true).into(),
                    &[Number::from_usize(*ctx, size).into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: Vector::new::<false>(ctx, size, fill).into() }
    }

    pub fn vector_ref(ctx: &Context<'gc>, vec: Value<'gc>, index: Value<'gc>) -> ThunkResult<'gc> {
        let Some(v) = vec.try_as::<Vector>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "vector-ref").into(),
                    Str::new(ctx, "not a vector", true).into(),
                    &[vec],
                )
            }
        };

        let Some(index) = index.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "vector-ref").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[index],
                )
            }
        };

        if !index.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "vector-ref").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        }

        let Some(index) = index.exact_integer_to_usize().filter(|_| !index.is_negative()) else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "vector-ref").into(),
                    Str::new(ctx, "not in usize range", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        };

        if index >= v.len() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "vector-ref").into(),
                    Str::new(ctx, "index out of range", true).into(),
                    &[Number::from_usize(*ctx, index).into_value(*ctx), Number::from_usize(*ctx, v.len()).into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: v[index].get() }
    }

    pub fn vector_set(ctx: &Context<'gc>, vec: Value<'gc>, index: Value<'gc>, new_value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(v) = vec.try_as::<Vector>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "vector-set!").into(),
                    Str::new(ctx, "not a vector", true).into(),
                    &[vec],
                )
            }
        };

        let Some(index) = index.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "vector-set!").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[index],
                )
            }
        };

        if !index.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "vector-set!").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        }

        let Some(index) = index.exact_integer_to_usize().filter(|_| !index.is_negative()) else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "vector-set!").into(),
                    Str::new(ctx, "not in usize range", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        };

        if index >= v.len() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "vector-set!").into(),
                    Str::new(ctx, "index out of range", true).into(),
                    &[Number::from_usize(*ctx, index).into_value(*ctx), Number::from_usize(*ctx, v.len()).into_value(*ctx)],
                )
            }
        }

        let wv = Gc::write(ctx, v);
        wv[index].unlock().set(new_value);

        ThunkResult { code: 0, value: Value::undefined() }
    }

    pub fn make_tuple(ctx: &Context<'gc>, size: Value<'gc>, fill: Value<'gc>) -> ThunkResult<'gc> {
        let Some(size) = size.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "make-tuple").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[size],
                )
            }
        };

        if !size.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "make-tuple").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[size.into_value(*ctx)],
                )
            }
        }

        let Some(size) = size.exact_integer_to_usize() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "make-tuple").into(),
                    Str::new(ctx, "not in usize range", true).into(),
                    &[size.into_value(*ctx)],
                )
            }
        };

        if size >= i32::MAX as usize {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "make-tuple").into(),
                    Str::new(ctx, "size too large", true).into(),
                    &[Number::from_usize(*ctx, size).into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: Tuple::new(ctx, size, fill).into() }
    }

    pub fn tuple_size(ctx: &Context<'gc>, tup: Value<'gc>) -> ThunkResult<'gc> {
        let Some(t) = tup.try_as::<Tuple>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tuple-size").into(),
                    Str::new(ctx, "not a tuple", true).into(),
                    &[tup],
                )
            }
        };

        ThunkResult { code: 0, value: Number::from_usize(*ctx, t.len()).into_value(*ctx) }
    }

    pub fn tuple_ref(ctx: &Context<'gc>, tup: Value<'gc>, index: Value<'gc>) -> ThunkResult<'gc> {
        let Some(t) = tup.try_as::<Tuple>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tuple-ref").into(),
                    Str::new(ctx, "not a tuple", true).into(),
                    &[tup],
                )
            }
        };

        let Some(index) = index.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tuple-ref").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[index],
                )
            }
        };

        if !index.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tuple-ref").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        }

        let Some(index) = index.exact_integer_to_usize().filter(|_| !index.is_negative()) else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tuple-ref").into(),
                    Str::new(ctx, "not in usize range", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        };

        if index >= t.len() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tuple-ref").into(),
                    Str::new(ctx, "index out of range", true).into(),
                    &[Number::from_usize(*ctx, index).into_value(*ctx), Number::from_usize(*ctx, t.len()).into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: t[index].get() }
    }

    pub fn tuple_set(ctx: &Context<'gc>, tup: Value<'gc>, index: Value<'gc>, new_value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(t) = tup.try_as::<Tuple>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tuple-set!").into(),
                    Str::new(ctx, "not a tuple", true).into(),
                    &[tup],
                )
            }
        };

        let Some(index) = index.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tuple-set!").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[index],
                )
            }
        };

        if !index.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tuple-set!").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        }

        let Some(index) = index.exact_integer_to_usize().filter(|_| !index.is_negative()) else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tuple-set!").into(),
                    Str::new(ctx, "not in usize range", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        };

        if index >= t.len() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "tuple-set!").into(),
                    Str::new(ctx, "index out of range", true).into(),
                    &[Number::from_usize(*ctx, index).into_value(*ctx), Number::from_usize(*ctx, t.len()).into_value(*ctx)],
                )
            }
        }

        let wt = Gc::write(ctx, t);
        wt[index].unlock().set(new_value);

        ThunkResult { code: 0, value: Value::undefined() }
    }

    pub fn variablep(ctx: &Context<'gc>, v: Value<'gc>) -> bool {
        v.is::<Variable>()
    }

    pub fn variable_ref(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(var) = v.try_as::<Variable>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "variable-ref").into(),
                    Str::new(ctx, "not a variable", true).into(),
                    &[v],
                )
            }
        };

        ThunkResult {
            code: 0,
            value: var.get()
        }
    }

    pub fn variable_set(ctx: &Context<'gc>, v: Value<'gc>, new_value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(var) = v.try_as::<Variable>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "variable-set!").into(),
                    Str::new(ctx, "not a variable", true).into(),
                    &[v, new_value],
                )
            }
        };

        var.set(*ctx, new_value);

        ThunkResult { code: 0, value: Value::undefined() }
    }

    pub fn variable_boundp(ctx: &Context<'gc>, v: Value<'gc>) -> ThunkResult<'gc> {
        let Some(var) = v.try_as::<Variable>() else {
            return ThunkResult { code: 1, value: make_assertion_violation(
                ctx,
                Symbol::from_str(*ctx, "variable-bound?").into(),
                Str::new(ctx, "not a variable", true).into(),
                &[v],
            )}
        };

        ThunkResult { code: 0, value: var.is_bound().into() }
    }

    pub fn define_(ctx: &Context<'gc>, name: Value<'gc>, value: Value<'gc>) -> ThunkResult<'gc> {
        let Some(sym) = name.try_as::<Symbol>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "define!").into(),
                    Str::new(ctx, "not a symbol", true).into(),
                    &[name],
                )
            }
        };

        let module = crate::runtime::modules::current_module(*ctx).get(*ctx);
        let Some(module) = module.try_as::<Module>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "define!").into(),
                    Str::new(ctx, "no current module", true).into(),
                    &[],
                )
            }
        };
        let var = module.define(*ctx, sym.into(), value);

        ThunkResult { code: 0, value: var.into() }
    }

    pub fn string_length(ctx: &Context<'gc>, s: Value<'gc>) -> ThunkResult<'gc> {
        let Some(s) = s.try_as::<Str>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-length").into(),
                    Str::new(ctx, "not a string", true).into(),
                    &[s],
                )
            }
        };

        ThunkResult { code: 0, value: Number::from_usize(*ctx, s.len()).into_value(*ctx) }
    }

    pub fn string_ref(ctx: &Context<'gc>, s: Value<'gc>, index: Value<'gc>) -> ThunkResult<'gc> {
        let Some(s) = s.try_as::<Str>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-ref").into(),
                    Str::new(ctx, "not a string", true).into(),
                    &[s],
                )
            }
        };

        let Some(index) = index.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-ref").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[index],
                )
            }
        };

        if !index.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-ref").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        }

        let Some(index) = index.exact_integer_to_usize().filter(|_| !index.is_negative()) else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-ref").into(),
                    Str::new(ctx, "not in usize range", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        };

        if index >= s.len() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-ref").into(),
                    Str::new(ctx, "index out of range", true).into(),
                    &[Number::from_usize(*ctx, index).into_value(*ctx), Number::from_usize(*ctx, s.len()).into_value(*ctx)],
                )
            }
        }

        ThunkResult { code: 0, value: Value::from_char(s.get(index).unwrap()) }
    }

    pub fn string_set(ctx: &Context<'gc>, s: Value<'gc>, index: Value<'gc>, new_char: Value<'gc>) -> ThunkResult<'gc> {
        let Some(s) = s.try_as::<Str>() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-set!").into(),
                    Str::new(ctx, "not a string", true).into(),
                    &[s],
                )
            }
        };

        let Some(index) = index.number() else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-set!").into(),
                    Str::new(ctx, "not a number", true).into(),
                    &[index],
                )
            }
        };

        if !index.is_exact_integer() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-set!").into(),
                    Str::new(ctx, "not an exact integer", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        }

        let Some(index) = index.exact_integer_to_usize().filter(|_| !index.is_negative()) else {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-set!").into(),
                    Str::new(ctx, "not in usize range", true).into(),
                    &[index.into_value(*ctx)],
                )
            }
        };

        if index >= s.len() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-set!").into(),
                    Str::new(ctx, "index out of range", true).into(),
                    &[Number::from_usize(*ctx, index).into_value(*ctx), Number::from_usize(*ctx, s.len()).into_value(*ctx)],
                )
            }
        }
        if !new_char.is_char() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "string-set!").into(),
                    Str::new(ctx, "not a character", true).into(),
                    &[new_char],
                )
            }
        }
        let c = new_char.char();

        Str::set(s, &ctx, index, c);

        ThunkResult { code: 0, value: Value::undefined() }
    }

    pub fn pre_write_barrier(
        ctx: &Context<'gc>,
        src: ObjectReference,
        offset: i32,
        target: ObjectReference
    ) -> () {
        unsafe {
            log::debug!("pre write barrier: src={:?}, offset={}, target={:?}", src, offset, target);
            ctx.mc.thread_unchecked()
                .mutator_unchecked()
                .barrier()
                .object_reference_write_pre(
                    src,
                    ObjectSlot::from_address(src.to_raw_address().offset(offset as _)),
                    Some(target)
                )
        }
    }

    pub fn pre_write_barrier_at_slot(
        ctx: &Context<'gc>,
        src: ObjectReference,
        slot: ObjectSlot,
        target: ObjectReference
    ) -> () {
        unsafe {
            log::debug!("pre write barrier: src={:?}, slot={:?}, target={:?}", src, slot, target);
            ctx.mc.thread_unchecked()
                .mutator_unchecked()
                .barrier()
                .object_reference_write_pre(
                    src,
                    slot,
                    Some(target)
                )
        }
    }

    pub fn post_write_barrier_slow(
        ctx: &Context<'gc>,
        src: ObjectReference,
        offset: i32,
        target: ObjectReference
    ) -> () {
        unsafe {
            log::debug!("post write barrier: src={:?}, offset={}, target={:?}", src, offset, target);
            ctx.mc.thread_unchecked()
                .mutator_unchecked()
                .barrier()
                .object_reference_write_slow(
                    src,
                    ObjectSlot::from_address(src.to_raw_address().offset(offset as _)),
                    Some(target)
                )
        }
    }

    pub fn post_write_barrier_at_slot(
        ctx: &Context<'gc>,
        src: ObjectReference,
        slot: ObjectSlot,
        target: ObjectReference
    ) -> () {
        unsafe {
            log::debug!("post write barrier: src={:?}, slot={:?}, target={:?}", src, slot, target);
            ctx.mc.thread_unchecked()
                .mutator_unchecked()
                .barrier()
                .object_reference_write_slow(
                    src,
                    slot,
                    Some(target)
                )
        }
    }

    // fixnums, most of them just return error code
    // since we try to inline them.
    pub fn fxplus(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fx+").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fx+").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxeq(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fx=").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fx=?").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxlt(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fx<?").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fx<?").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxgt(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fx>?").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fx>?").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxle(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fx<=?").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fx<=?").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxge(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fx>=?").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fx>=?").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxzero(
        ctx: &Context<'gc>,
        x: Value<'gc>
    ) -> ThunkResult<'gc> {
        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxzero?").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[x],
            )
        }
    }

    pub fn fxpositive(
        ctx: &Context<'gc>,
        x: Value<'gc>
    ) -> ThunkResult<'gc> {
        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxpositive?").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[x],
            )
        }
    }

    pub fn fxnegative(
        ctx: &Context<'gc>,
        x: Value<'gc>
    ) -> ThunkResult<'gc> {
        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxnegative?").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[x],
            )
        }
    }

    pub fn fxodd(
        ctx: &Context<'gc>,
        x: Value<'gc>
    ) -> ThunkResult<'gc> {
        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxodd?").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[x],
            )
        }
    }

    pub fn fxeven(
        ctx: &Context<'gc>,
        x: Value<'gc>
    ) -> ThunkResult<'gc> {
        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxeven?").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[x],
            )
        }
    }

    pub fn fxmax(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxmax").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxmax").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxmin(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxmin").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxmin").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxtimes(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fx*").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fx*").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxminus(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fx-").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fx-").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxdiv(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fx/").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        if let Some(Number::Fixnum(0)) = y.number() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fx/").into(),
                    Str::new(ctx, "division by zero", true).into(),
                    &[y],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fx/").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxmod(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxmod").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        if let Some(Number::Fixnum(0)) = y.number() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxmod").into(),
                    Str::new(ctx, "division by zero", true).into(),
                    &[y],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxmod").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxdiv_and_mod(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxdiv-and-mod").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        if let Some(Number::Fixnum(0)) = y.number() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxdiv-and-mod").into(),
                    Str::new(ctx, "division by zero", true).into(),
                    &[y],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxdiv-and-mod").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxdiv0(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxdiv0").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        if let Some(Number::Fixnum(0)) = y.number() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxdiv0").into(),
                    Str::new(ctx, "division by zero", true).into(),
                    &[y],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxdiv0").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxmod0(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxmod0").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        if let Some(Number::Fixnum(0)) = y.number() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxmod0").into(),
                    Str::new(ctx, "division by zero", true).into(),
                    &[y],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxmod0").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxdiv0_and_mod0(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxdiv0-and-mod0").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        if let Some(Number::Fixnum(0)) = y.number() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxdiv0-and-mod0").into(),
                    Str::new(ctx, "division by zero", true).into(),
                    &[y],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxdiv0-and-mod0").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxnot(
        ctx: &Context<'gc>,
        x: Value<'gc>
    ) -> ThunkResult<'gc> {
        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxnot").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[x],
            )
        }
    }

    pub fn fxior(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxior").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxior").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxxor(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxxor").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxxor").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[y],
            )
        }
    }

    pub fn fxif(
        ctx: &Context<'gc>,
        test: Value<'gc>,
        then: Value<'gc>,
        else_: Value<'gc>
    ) -> ThunkResult<'gc> {
        if !test.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxif").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[test],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxif").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[then, else_],
            )
        }
    }

    pub fn fxbit_count(
        ctx: &Context<'gc>,
        x: Value<'gc>
    ) -> ThunkResult<'gc> {


        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxbit-count").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[x],
            )
        }
    }

    pub fn fxlength(
        ctx: &Context<'gc>,
        x: Value<'gc>
    ) -> ThunkResult<'gc> {

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxlength").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[x],
            )
        }
    }

    pub fn fxfirst_bit_set(
        ctx: &Context<'gc>,
        x: Value<'gc>
    ) -> ThunkResult<'gc> {

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxfirst-bit-set").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[x],
            )
        }
    }

    pub fn fxbit_setp(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        index: Value<'gc>
    ) -> ThunkResult<'gc> {

        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxbit-set?").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxbit-set?").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[index],
            )
        }
    }

    pub fn fxcopy_bit(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        index: Value<'gc>,
        value: Value<'gc>
    ) -> ThunkResult<'gc> {

        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxcopy-bit").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxcopy-bit").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[index, value],
            )
        }
    }

    pub fn fxcopy_bit_field(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        start: Value<'gc>,
        width: Value<'gc>,
        value: Value<'gc>
    ) -> ThunkResult<'gc> {

        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxcopy-bit-field").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxcopy-bit-field").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[start, width, value],
            )
        }
    }

    pub fn fxarithmetic_shift(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        count: Value<'gc>
    ) -> ThunkResult<'gc> {

        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxarithmetic-shift").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxarithmetic-shift").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[count],
            )
        }
    }

    pub fn fxarithmetic_shift_left(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        count: Value<'gc>
    ) -> ThunkResult<'gc> {

        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxarithmetic-shift-left").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxarithmetic-shift-left").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[count],
            )
        }
    }

    pub fn fxarithmetic_shift_right(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        count: Value<'gc>
    ) -> ThunkResult<'gc> {

        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxarithmetic-shift-right").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxarithmetic-shift-right").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[count],
            )
        }
    }

    pub fn fxrotate_bit_field(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        start: Value<'gc>,
        width: Value<'gc>,
        count: Value<'gc>
    ) -> ThunkResult<'gc> {

        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxrotate-bit-field").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxrotate-bit-field").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[start, width, count],
            )
        }
    }

    pub fn fxreverse_bit_field(
        ctx: &Context<'gc>,
        x: Value<'gc>,
        start: Value<'gc>,
        end: Value<'gc>
    ) -> ThunkResult<'gc> {

        if !x.is_int32() {
            return ThunkResult {
                code: 1,
                value: make_assertion_violation(ctx,
                    Symbol::from_str(*ctx, "fxreverse-bit-field").into(),
                    Str::new(ctx, "not a fixnum", true).into(),
                    &[x],
                )
            }
        }

        return ThunkResult {
            code: 1, value:
            make_assertion_violation(ctx,
                Symbol::from_str(*ctx, "fxreverse-bit-field").into(),
                Str::new(ctx, "not a fixnum", true).into(),
                &[start, end],
            )
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

/*
    constructors for exceptions. Right now rely on the fact
    of invoking Scheme code from Rust. It is error prone and we ideally
    should construct them directly. Leave it as is for simplicity for now...

*/

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
        .unwrap_or_else(|| {
            panic!("pre boot code, who={who}, message={message}, irritants={irritants:?}",)
        });

    match call_scheme(*ctx, assertion_violation, args) {
        VMResult::Ok(val) => val,
        VMResult::Err(err) => err,
        VMResult::Yield => unreachable!(),
    }
}

pub fn make_undefined_violation<'gc>(
    ctx: &Context<'gc>,
    who: Value<'gc>,
    message: impl AsRef<str>,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let message: Value = Str::new(&ctx, message, true).into();
    let mut args = vec![who, message];
    args.extend_from_slice(irritants);
    let undefined_violation = root_module(*ctx)
        .get(
            *ctx,
            Symbol::from_str(*ctx, ".make-undefined-violation").into(),
        )
        .unwrap_or_else(|| {
            panic!("pre boot code, who={who}, message={message}, irritants={irritants:?}",)
        });
    match call_scheme(*ctx, undefined_violation, args) {
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

pub fn resolve_module<'gc>(ctx: Context<'gc>, name: Value<'gc>, public: bool) -> ThunkResult<'gc> {
    let Some(module) = crate::runtime::modules::resolve_module(ctx, name, false, false) else {
        return ThunkResult {
            code: 1,
            value: make_undefined_violation(
                &ctx,
                Symbol::from_str(ctx, "resolve-module").into(),
                &format!("module '{name}' not found"),
                &[name],
            ),
        };
    };

    ThunkResult {
        code: 0,
        value: if public {
            module.public_interface.get().unwrap().into()
        } else {
            module.into()
        },
    }
}
