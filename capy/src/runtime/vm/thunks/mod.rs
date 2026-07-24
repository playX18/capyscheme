//! VM thunks -- functions directly callable from compiled code.
//!
//!
//! This file largely copies bunch of functions from other places
//! in order to make them callable from compiled code.

#![allow(dead_code, unused_variables)]

pub mod control;
pub mod gc;
pub mod helpers;
pub mod misc;
pub mod modules;
pub mod numbers;
pub mod pairs;
pub mod preds;
pub mod vectors;

pub use helpers::{
    RegisterArgs, collect_register_args, save_register_args, wrong_number_of_args_impl,
};

use crate::rsgc::{
    ObjectSlot,
    mmtk::util::{Address, ObjectReference},
};
use crate::runtime::vm::exceptions::make_undefined_violation as undefined_violation;
use crate::{
    prelude::ClosureRef,
    runtime::{
        Context,
        value::{ReturnCode, Str, Symbol, Value},
        vm::{ExecutionResult, call_scheme, thunk_raise},
    },
};

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ThunkResult<'gc> {
    pub code: usize,
    pub value: Value<'gc>,
}

macro_rules! thunks {
    ($gl: lifetime: $(
        $(#[$attr:meta])*
        $v: vis fn $name : ident($($arg: ident : $t: ty),*) -> $ret: ty $b : block
    )*) => {
        $(
            $(#[$attr])*
            #[unsafe(export_name=concat!("capy_thunks_", stringify!($name)))]
            $v extern "C" fn $name<$gl>($($arg: $t),*) -> $ret $b
        )*

        pub struct ImportedThunks {
            $(
                pub $name: cranelift_codegen::ir::entities::FuncRef
            ),*
        }

        paste::paste! {
            #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
            #[allow(non_camel_case_types)]
            #[repr(u32)]
            pub enum RuntimeThunk {
                $(
                    [<Thunk_ $name>],
                )*
            }

            impl RuntimeThunk {
                pub const ALL: &'static [Self] = &[
                    $(
                        Self::[<Thunk_ $name>],
                    )*
                ];

                pub fn id(self) -> u32 {
                    self as u32
                }

                pub fn from_id(id: u32) -> Option<Self> {
                    match id {
                        $(
                            x if x == Self::[<Thunk_ $name>] as u32 => {
                                Some(Self::[<Thunk_ $name>])
                            }
                        )*
                        _ => None,
                    }
                }

                pub fn address(self) -> Address {
                    match self {
                        $(
                            Self::[<Thunk_ $name>] => Address::from_ptr($name as *const ()),
                        )*
                    }
                }
            }
        }

        impl<$gl> ImportedThunks {
            pub fn new_direct(
                function: &mut cranelift_codegen::ir::function::Function,
            ) -> Self {
                let callconv = cranelift_codegen::isa::CallConv::SystemV;
                let mut sig = cranelift_codegen::ir::Signature::new(callconv);
                paste::paste! {
                    $(
                        $(
                            for ty in < $t as compiler::PrimType>::clif_type() {
                                sig.params.push(cranelift_codegen::ir::AbiParam::new(ty));
                            }
                        )*

                        for ty in <$ret as compiler::PrimType>::clif_type() {
                            sig.returns.push(cranelift_codegen::ir::AbiParam::new(ty));
                        }

                        let sig_ref = function.import_signature(sig.clone());
                        let $name = crate::compiler::codegen::declare_function(
                            function,
                            crate::compiler::codegen::Symbol::imported(
                                crate::compiler::codegen::ImportKind::RuntimeThunk,
                                crate::compiler::codegen::ImportedSymbol::new(
                                    RuntimeThunk::[<Thunk_ $name>].id(),
                                ),
                            ),
                            sig_ref,
                            false,
                        );
                        sig.clear(callconv);
                    )*
                }

                Self {
                    $($name),*
                }
            }
        }
    };

}

pub mod compiler {
    use crate::{
        rsgc::{
            Gc, ObjectSlot,
            mmtk::util::{Address, ObjectReference},
        },
        runtime::Context,
    };
    use cranelift_codegen::ir;

    use crate::runtime::{value::Value, vm::thunks::ThunkResult};

    pub trait PrimType {
        fn clif_type() -> impl Iterator<Item = ir::Type>;
    }

    impl PrimType for ObjectSlot {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }

    impl PrimType for i64 {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }

    impl PrimType for f64 {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::F64)
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

    impl PrimType for u32 {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I32)
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

    impl<'gc> PrimType for Context<'gc> {
        fn clif_type() -> impl Iterator<Item = ir::Type> {
            std::iter::once(ir::types::I64)
        }
    }
}

thunks! {
    'gc:

    fn wrong_number_of_args(ctx: Context<'gc>,
        subr: Value<'gc>,
        got: usize,
        expected: isize,
        rands: *const Value<'gc>) -> Value<'gc> {
        control::wrong_number_of_args(ctx, subr, got, expected, rands)
    }

    pub fn cons_rest_regs(ctx: Context<'gc>,
        argc: usize,
        arg0: Value<'gc>,
        arg1: Value<'gc>,
        arg2: Value<'gc>,
        arg3: Value<'gc>,
        overflow: *const Value<'gc>,
        from: usize) -> Value<'gc> {
        control::cons_rest_regs(ctx, argc, arg0, arg1, arg2, arg3, overflow, from)
    }

    pub fn raise_condition_regs(ctx: Context<'gc>,
        code: usize,
        argc: usize,
        arg0: Value<'gc>,
        arg1: Value<'gc>,
        arg2: Value<'gc>,
        arg3: Value<'gc>,
        overflow: *const Value<'gc>,
        from: usize) -> Value<'gc> {
        control::raise_condition_regs(ctx, code, argc, arg0, arg1, arg2, arg3, overflow, from)
    }

    pub fn raise_condition_with_source_regs(ctx: Context<'gc>,
        code: usize,
        argc: usize,
        arg0: Value<'gc>,
        arg1: Value<'gc>,
        arg2: Value<'gc>,
        arg3: Value<'gc>,
        overflow: *const Value<'gc>,
        from: usize) -> Value<'gc> {
        control::raise_condition_with_source_regs(ctx, code, argc, arg0, arg1, arg2, arg3, overflow, from)
    }

    pub fn non_applicable(ctx: Context<'gc>,
        subr: Value<'gc>) -> Value<'gc> {
        control::non_applicable(ctx, subr)
    }

    pub fn generic_apply_regs(ctx: Context<'gc>,
        generic: Value<'gc>,
        argc: usize,
        arg0: Value<'gc>,
        arg1: Value<'gc>,
        arg2: Value<'gc>,
        arg3: Value<'gc>,
        overflow: *const Value<'gc>,
        has_retk: u8) -> ThunkResult<'gc> {
        control::generic_apply_regs(ctx, generic, argc, arg0, arg1, arg2, arg3, overflow, has_retk)
    }

    pub fn make_variable(ctx: Context<'gc>,
        value: Value<'gc>) -> Value<'gc> {
        modules::make_variable(ctx, value)
    }

    pub fn lookup_bound(ctx: Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> Value<'gc> {
        modules::lookup_bound(ctx, module, name)
    }

    pub fn lookup(ctx: Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> Value<'gc> {
        modules::lookup(ctx, module, name)
    }

    pub fn lookup_bound_public(ctx: Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> Value<'gc> {
        modules::lookup_bound_public(ctx, module, name)
    }

    pub fn lookup_bound_private(ctx: Context<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> Value<'gc> {
        modules::lookup_bound_private(ctx, module, name)
    }

    pub fn define(ctx: Context<'gc>,
        name: Value<'gc>,
        value: Value<'gc>) -> Value<'gc> {
        modules::define(ctx, name, value)
    }

    pub fn current_module(ctx: Context<'gc>) -> Value<'gc> {
        modules::current_module(ctx)
    }

    pub fn set_current_module(ctx: Context<'gc>, module: Value<'gc>) -> Value<'gc> {
        modules::set_current_module(ctx, module)
    }

    pub fn module_ensure_local_variable(ctx: Context<'gc>, module: Value<'gc>, name: Value<'gc>) -> Value<'gc> {
        modules::module_ensure_local_variable(ctx, module, name)
    }

    pub fn yieldpoint_block(ctx: Context<'gc>,
        rator: Value<'gc>,
        argc: usize,
        arg0: Value<'gc>,
        arg1: Value<'gc>,
        arg2: Value<'gc>,
        arg3: Value<'gc>) -> () {
        gc::yieldpoint_block(ctx, rator, argc, arg0, arg1, arg2, arg3)
    }

    /// Exit CPSed code back to the Rust `setjmp` in [`crate::runtime::vm::trampoline`].
    pub fn scheme_longjmp(ctx: Context<'gc>, code: usize, value: Value<'gc>) -> () {
        let code = match code {
            x if x == ReturnCode::ReturnOk as usize => ReturnCode::ReturnOk,
            x if x == ReturnCode::ReturnErr as usize => ReturnCode::ReturnErr,
            x if x == ReturnCode::Raise as usize => ReturnCode::Raise,
            x if x == ReturnCode::Continue as usize => ReturnCode::Continue,
            _ => ReturnCode::ReturnErr,
        };
        // SAFETY: Only callable while an active Scheme entry has installed `exit_jmp`.
        unsafe { crate::runtime::vm::scheme_longjmp(ctx, code, value) }
    }

    pub fn reverse(ctx: Context<'gc>, list: Value<'gc>) -> Value<'gc> {
        pairs::reverse(ctx, list)
    }

    pub fn eqv(a: Value<'gc>, b: Value<'gc>) -> bool {
        preds::eqv(a, b)
    }

    pub fn equal(a: Value<'gc>, b: Value<'gc>) -> bool {
        preds::equal(a, b)
    }

    pub fn is_rational(value: Value<'gc>) -> bool {
        preds::is_rational(value)
    }

    pub fn is_zero(ctx: Context<'gc>, value: Value<'gc>) -> Value<'gc> {
        numbers::is_zero(ctx, value)
    }

    pub fn negate(ctx: Context<'gc>, value: Value<'gc>) -> Value<'gc> {
        numbers::negate(ctx, value)
    }

    pub fn memv(ctx: Context<'gc>, key: Value<'gc>, list: Value<'gc>) -> Value<'gc> {
        numbers::memv(ctx, key, list)
    }

    pub fn memq(ctx: Context<'gc>, key: Value<'gc>, list: Value<'gc>) -> Value<'gc> {
        numbers::memq(ctx, key, list)
    }

    pub fn number_eq(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_eq(ctx, a, b)
    }

    pub fn number_lt(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_lt(ctx, a, b)
    }

    pub fn number_gt(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_gt(ctx, a, b)
    }

    pub fn number_le(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_le(ctx, a, b)
    }

    pub fn number_ge(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_ge(ctx, a, b)
    }

    pub fn string2symbol(ctx: Context<'gc>, s: Value<'gc>) -> Value<'gc> {
        misc::string2symbol(ctx, s)
    }

    pub fn symbol2string(ctx: Context<'gc>, s: Value<'gc>) -> Value<'gc> {
        misc::symbol2string(ctx, s)
    }

    pub fn number_plus(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_plus(ctx, a, b)
    }

    pub fn number_minus(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_minus(ctx, a, b)
    }

    pub fn number_times(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_times(ctx, a, b)
    }

    pub fn number_div(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_div(ctx, a, b)
    }

    pub fn quotient(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::quotient(ctx, a, b)
    }

    pub fn remainder(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::remainder(ctx, a, b)
    }

    pub fn modulo(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::modulo(ctx, a, b)
    }

    pub fn exact2inexact(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::exact2inexact(ctx, n)
    }

    pub fn inexact_to_exact(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::inexact_to_exact(ctx, n)
    }

    pub fn expt(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::expt(ctx, a, b)
    }

    pub fn ash(ctx: Context<'gc>, n: Value<'gc>, count: Value<'gc>) -> Value<'gc> {
        numbers::ash(ctx, n, count)
    }

    pub fn logand(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::logand(ctx, a, b)
    }

    pub fn logior(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::logior(ctx, a, b)
    }

    pub fn logxor(ctx: Context<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        misc::logxor(ctx, a, b)
    }

    pub fn lognot(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::lognot(ctx, n)
    }

    pub fn sqrt(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::sqrt(ctx, n)
    }

    pub fn abs(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::abs(ctx, n)
    }

    pub fn floor(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::floor(ctx, n)
    }

    pub fn ceiling(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::ceiling(ctx, n)
    }

    pub fn truncate(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::truncate(ctx, n)
    }

    pub fn sin(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::sin(ctx, n)
    }

    pub fn cos(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::cos(ctx, n)
    }

    pub fn tan(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::tan(ctx, n)
    }

    pub fn asin(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::asin(ctx, n)
    }

    pub fn acos(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::acos(ctx, n)
    }

    pub fn atan(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::atan(ctx, n)
    }

    pub fn exp(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::exp(ctx, n)
    }

    pub fn log(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::log(ctx, n)
    }

    pub fn atan2(ctx: Context<'gc>, y: Value<'gc>, x: Value<'gc>) -> Value<'gc> {
        numbers::atan2(ctx, y, x)
    }

    pub fn listp(v: Value<'gc>) -> bool {
        preds::listp(v)
    }

    pub fn complexp(v: Value<'gc>) -> bool {
        preds::complexp(v)
    }

    pub fn realp(v: Value<'gc>) -> bool {
        preds::realp(v)
    }

    pub fn nanp(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
        preds::nanp(ctx, v)
    }

    pub fn integerp(ctx: Context<'gc>, v: Value<'gc>) -> bool {
        preds::integerp(ctx, v)
    }

    pub fn exactp(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
        preds::exactp(ctx, v)
    }

    pub fn inexactp(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
        preds::inexactp(ctx, v)
    }

    pub fn evenp(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
        numbers::evenp(ctx, v)
    }

    pub fn oddp(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
        numbers::oddp(ctx, v)
    }

    pub fn exact_integerp(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
        preds::exact_integerp(ctx, v)
    }

    pub fn char_to_integer(ctx: Context<'gc>, c: Value<'gc>) -> Value<'gc> {
        misc::char_to_integer(ctx, c)
    }

    pub fn integer_to_char(ctx: Context<'gc>, n: Value<'gc>) -> Value<'gc> {
        misc::integer_to_char(ctx, n)
    }

    pub fn append(ctx: Context<'gc>, m1: Value<'gc>, m2: Value<'gc>) -> Value<'gc> {
        pairs::append(ctx, m1, m2)
    }


    pub fn length(ctx: Context<'gc>, v: Value<'gc>) -> Value<'gc> {
        pairs::length(ctx, v)
    }

    pub fn make_vector(ctx: Context<'gc>, size: Value<'gc>, fill: Value<'gc>) -> Value<'gc> {
        vectors::make_vector(ctx, size, fill)
    }

    pub fn make_tuple(ctx: Context<'gc>, size: Value<'gc>, fill: Value<'gc>) -> Value<'gc> {
        vectors::make_tuple(ctx, size, fill)
    }

    pub fn vector_ref(ctx: Context<'gc>, vec: Value<'gc>, index: Value<'gc>) -> Value<'gc> {
        vectors::vector_ref(ctx, vec, index)
    }

    pub fn string_ref(ctx: Context<'gc>, s: Value<'gc>, index: Value<'gc>) -> Value<'gc> {
        misc::string_ref_value(ctx, s, index)
    }

    pub fn string_ref_unchecked(ctx: Context<'gc>, s: Value<'gc>, index: Value<'gc>) -> Value<'gc> {
        misc::string_ref_unchecked_value(ctx, s, index)
    }

    pub fn bytevector_length(ctx: Context<'gc>, bv: Value<'gc>) -> Value<'gc> {
        vectors::bytevector_length(ctx, bv)
    }

    pub fn bytevector_u8_ref(ctx: Context<'gc>, bv: Value<'gc>, index: Value<'gc>) -> Value<'gc> {
        vectors::bytevector_u8_ref(ctx, bv, index)
    }

    pub fn vector_set(ctx: Context<'gc>, vec: Value<'gc>, index: Value<'gc>, new_value: Value<'gc>) -> Value<'gc> {
        vectors::vector_set(ctx, vec, index, new_value)
    }

    pub fn tuple_size(ctx: Context<'gc>, tup: Value<'gc>) -> Value<'gc> {
        vectors::tuple_size(ctx, tup)
    }

    pub fn push_cframe(ctx: Context<'gc>,
        key: Value<'gc>,
        mark: Value<'gc>,
        retk: ClosureRef<'gc>) -> Value<'gc> {
        misc::push_cframe(ctx, key, mark, retk)
    }

    pub fn current_continuation_marks(ctx: Context<'gc>) -> Value<'gc> {
        misc::current_continuation_marks(ctx)
    }

    pub fn set_attachments(ctx: Context<'gc>,
        marks: Value<'gc>) -> Value<'gc> {
        misc::set_attachments(ctx, marks)
    }

    pub fn make_syntax(ctx: Context<'gc>,
        exp: Value<'gc>,
        wrap: Value<'gc>,
        module: Value<'gc>,
        source: Value<'gc>,
        properties: Value<'gc>) -> Value<'gc> {
        misc::make_syntax(ctx, exp, wrap, module, source, properties)
    }

    pub fn push_dframe_regs(ctx: Context<'gc>,
        src: Value<'gc>,
        rator: Value<'gc>,
        argc: usize,
        arg0: Value<'gc>,
        arg1: Value<'gc>,
        arg2: Value<'gc>,
        arg3: Value<'gc>,
        overflow: *const Value<'gc>) -> Value<'gc> {
        control::push_dframe_regs(ctx, src, rator, argc, arg0, arg1, arg2, arg3, overflow)
    }

    pub fn push_debug_dframe_regs(ctx: Context<'gc>,
        rator: Value<'gc>,
        argc: usize,
        arg0: Value<'gc>,
        arg1: Value<'gc>,
        arg2: Value<'gc>,
        arg3: Value<'gc>,
        overflow: *const Value<'gc>) -> Value<'gc> {
        control::push_debug_dframe_regs(ctx, rator, argc, arg0, arg1, arg2, arg3, overflow)
    }

    pub fn default_retk(ctx: Context<'gc>) -> Value<'gc> {
        control::default_retk(ctx)
    }

    pub fn exception_handler(ctx: Context<'gc>) -> Value<'gc> {
        control::exception_handler(ctx)
    }

    pub fn fxeq(ctx: Context<'gc>,
        x: Value<'gc>,
        y: Value<'gc>) -> Value<'gc> {
        numbers::fxeq(ctx, x, y)
    }

    pub fn pre_write_barrier_at_slot(ctx: Context<'gc>,
        src: ObjectReference,
        slot: ObjectSlot,
        target: ObjectReference) -> () {
        gc::pre_write_barrier_at_slot(ctx, src, slot, target)
    }

    pub fn post_write_barrier_at_slot(ctx: Context<'gc>,
        src: ObjectReference,
        slot: ObjectSlot,
        target: ObjectReference) -> () {
        gc::post_write_barrier_at_slot(ctx, src, slot, target)
    }

    pub fn post_write_barrier_slow(ctx: Context<'gc>,
        src: ObjectReference,
        offset: i32,
        target: ObjectReference) -> () {
        gc::post_write_barrier_slow(ctx, src, offset, target)
    }

    pub fn alloc_with_header_word(ctx: Context<'gc>,
        header_word: usize,
        size: usize) -> Value<'gc> {
        gc::alloc_with_header_word(ctx, header_word, size)
    }

    pub fn fl_ceiling(x: f64) -> f64 {
        libm::ceil(x)
    }

    pub fn fl_floor(x: f64) -> f64 {
        libm::floor(x)
    }

    pub fn fl_truncate(x: f64) -> f64 {
        libm::trunc(x)
    }

    pub fn fl_round(x: f64) -> f64 {
        libm::rint(x)
    }

}

// SAFETY: Invariants are upheld at this call site
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
    ctx: Context<'gc>,
    who: Value<'gc>,
    message: Value<'gc>,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let args = std::iter::once(who)
        .chain(std::iter::once(message))
        .chain(irritants.iter().cloned())
        .collect::<Vec<_>>();

    let assertion_violation = ctx
        .globals()
        .root_module()
        .get(
            ctx,
            Symbol::from_str(ctx, "%make-assertion-violation").into(),
        )
        .unwrap_or_else(|| {
            panic!("failed to resolve %make-assertion-violation (pre-boot): who={who}, message={message}, irritants={irritants:?}",)
        });

    match call_scheme(ctx, assertion_violation, args) {
        ExecutionResult::Ok(val) => val,
        ExecutionResult::Err(err) => err,
    }
}

pub fn make_undefined_violation<'gc>(
    ctx: Context<'gc>,
    who: Value<'gc>,
    message: impl AsRef<str>,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let message: Value = Str::new(*ctx, message, true).into();
    let mut args = vec![who, message];
    args.extend_from_slice(irritants);
    let undefined_violation = ctx
        .globals()
        .root_module()
        .get(
            ctx,
            Symbol::from_str(ctx, "%make-undefined-violation").into(),
        )
        .unwrap_or_else(|| {
            panic!("failed to resolve %make-undefined-violation (pre-boot): who={who}, message={message}, irritants={irritants:?}",)
        });
    match call_scheme(ctx, undefined_violation, args) {
        ExecutionResult::Ok(val) => val,
        ExecutionResult::Err(err) => err,
    }
}

pub fn make_error<'gc>(
    ctx: Context<'gc>,
    who: Value<'gc>,
    message: Value<'gc>,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let args = std::iter::once(who)
        .chain(std::iter::once(message))
        .chain(irritants.iter().cloned())
        .collect::<Vec<_>>();

    let error = ctx
        .globals()
        .root_module()
        .get(ctx, Symbol::from_str(ctx, "%make-error").into())
        .expect("failed to resolve %make-error (pre-boot)");

    match call_scheme(ctx, error, args) {
        ExecutionResult::Ok(val) => val,
        ExecutionResult::Err(err) => err,
    }
}

pub fn make_io_error<'gc>(
    ctx: Context<'gc>,
    who: &str,
    message: Value<'gc>,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let who = Symbol::from_str(ctx, who).into();
    let args = std::iter::once(who)
        .chain(std::iter::once(message))
        .chain(irritants.iter().cloned())
        .collect::<Vec<_>>();

    let io_error = ctx
        .globals()
        .root_module()
        .get(ctx, Symbol::from_str(ctx, "%make-io-error").into())
        .unwrap_or_else(|| {
            panic!(
                "failed to resolve %make-io-error (pre-boot): who={who}, message={message}, irritants={:?}",
                irritants
            )
        });

    match call_scheme(ctx, io_error, args) {
        ExecutionResult::Ok(val) => val,
        ExecutionResult::Err(err) => err,
    }
}

pub fn make_lexical_violation<'gc>(
    ctx: Context<'gc>,
    who: &str,
    message: impl AsRef<str>,
) -> Value<'gc> {
    let who = Symbol::from_str(ctx, who).into();
    let message: Value = Str::new(*ctx, message, true).into();
    let args = vec![who, message];

    let lexical_violation = ctx
        .globals()
        .root_module()
        .get(ctx, Symbol::from_str(ctx, "%make-lexical-violation").into())
        .unwrap_or_else(|| panic!("failed to resolve %make-lexical-violation (pre-boot): who={who}, message={message}",));

    match call_scheme(ctx, lexical_violation, args) {
        ExecutionResult::Ok(val) => val,
        ExecutionResult::Err(err) => err,
    }
}

pub fn resolve_module<'gc>(ctx: Context<'gc>, name: Value<'gc>, public: bool) -> Value<'gc> {
    let Some(module) = crate::runtime::modules::resolve_module(ctx, name, false, false) else {
        thunk_raise(
            ctx,
            undefined_violation(
                ctx,
                Some(Symbol::from_str(ctx, "resolve-module").into()),
                &format!("module '{name}' not found"),
                &[name],
            ),
        );
    };

    if public {
        module.public_interface.get().expect("index in range").into()
    } else {
        module.into()
    }
}

#[cfg(test)]
mod runtime_thunk_import_tests {
    use super::*;

    #[test]
    fn direct_imported_thunks_use_runtime_thunk_ids() {
        let mut function = cranelift_codegen::ir::Function::new();
        let imported = ImportedThunks::new_direct(&mut function);

        let func = &function.dfg.ext_funcs[imported.wrong_number_of_args];
        let cranelift_codegen::ir::ExternalName::User(name_ref) = func.name else {
            panic!("direct thunk import should use a user external name");
        };
        let symbol = crate::compiler::codegen::Symbol::from_external_name(
            function.params.user_named_funcs()[name_ref].clone(),
        );

        assert_eq!(
            symbol,
            Some(crate::compiler::codegen::Symbol::imported(
                crate::compiler::codegen::ImportKind::RuntimeThunk,
                crate::compiler::codegen::ImportedSymbol::new(
                    RuntimeThunk::Thunk_wrong_number_of_args.id(),
                ),
            ))
        );
    }
}
