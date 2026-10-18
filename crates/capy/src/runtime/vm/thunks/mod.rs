//! VM thunks: functions directly callable from compiled code.
//!
//!
//! This file largely copies bunch of functions from other places
//! in order to make them callable from compiled code.

pub mod control;
pub mod gc;
pub mod helpers;
pub mod misc;
pub mod modules;
pub mod numbers;
pub mod pairs;
pub mod preds;
pub mod vectors;

pub use helpers::wrong_number_of_args_impl;

use crate::heap::{ObjectSlot, mmtk::util::ObjectReference};
use crate::runtime::vm::exceptions::make_undefined_violation as undefined_violation;
use crate::runtime::{
    MutationContext,
    value::{Str, Symbol, Value},
    vm::{ExecutionResult, thunk_raise},
};

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ThunkResult<'gc> {
    pub code: usize,
    pub value: Value<'gc>,
}

/// Define the runtime entry points the interpreter calls for primitives.
/// Each one is an ordinary function, the macro only supplies the `'gc`
/// lifetime so the list reads as a table.
macro_rules! thunks {
    ($gl: lifetime: $(
        $(#[$attr:meta])*
        $v: vis fn $name : ident($($arg: ident : $t: ty),*) -> $ret: ty $b : block
    )*) => {
        $(
            $(#[$attr])*
            #[allow(clippy::too_many_arguments)]
            pub fn $name<$gl>($($arg: $t),*) -> $ret $b
        )*
    };
}

thunks! {
    'gc:

    pub fn non_applicable(ctx: MutationContext<'gc>,
        subr: Value<'gc>) -> Value<'gc> {
        control::non_applicable(ctx, subr)
    }

    pub fn make_variable(ctx: MutationContext<'gc>,
        value: Value<'gc>) -> Value<'gc> {
        modules::make_variable(ctx, value)
    }

    pub fn lookup_bound(ctx: MutationContext<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> Value<'gc> {
        modules::lookup_bound(ctx, module, name)
    }

    pub fn lookup(ctx: MutationContext<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> Value<'gc> {
        modules::lookup(ctx, module, name)
    }

    pub fn lookup_bound_public(ctx: MutationContext<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> Value<'gc> {
        modules::lookup_bound_public(ctx, module, name)
    }

    pub fn lookup_bound_private(ctx: MutationContext<'gc>,
        module: Value<'gc>,
        name: Value<'gc>) -> Value<'gc> {
        modules::lookup_bound_private(ctx, module, name)
    }

    pub fn define(ctx: MutationContext<'gc>,
        name: Value<'gc>,
        value: Value<'gc>) -> Value<'gc> {
        modules::define(ctx, name, value)
    }

    pub fn current_module(ctx: MutationContext<'gc>) -> Value<'gc> {
        modules::current_module(ctx)
    }

    pub fn set_current_module(ctx: MutationContext<'gc>, module: Value<'gc>) -> Value<'gc> {
        modules::set_current_module(ctx, module)
    }

    pub fn module_ensure_local_variable(ctx: MutationContext<'gc>, module: Value<'gc>, name: Value<'gc>) -> Value<'gc> {
        modules::module_ensure_local_variable(ctx, module, name)
    }

    pub fn yieldpoint_block(ctx: MutationContext<'gc>) -> () {
        gc::yieldpoint_block(ctx)
    }

    /// Exit CPSed code back to the Rust `setjmp` in [`crate::runtime::vm::trampoline`].
    pub fn reverse(ctx: MutationContext<'gc>, list: Value<'gc>) -> Value<'gc> {
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

    pub fn is_zero(ctx: MutationContext<'gc>, value: Value<'gc>) -> Value<'gc> {
        numbers::is_zero(ctx, value)
    }

    pub fn negate(ctx: MutationContext<'gc>, value: Value<'gc>) -> Value<'gc> {
        numbers::negate(ctx, value)
    }

    pub fn memv(ctx: MutationContext<'gc>, key: Value<'gc>, list: Value<'gc>) -> Value<'gc> {
        numbers::memv(ctx, key, list)
    }

    pub fn memq(ctx: MutationContext<'gc>, key: Value<'gc>, list: Value<'gc>) -> Value<'gc> {
        numbers::memq(ctx, key, list)
    }

    pub fn number_eq(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_eq(ctx, a, b)
    }

    pub fn number_lt(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_lt(ctx, a, b)
    }

    pub fn number_gt(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_gt(ctx, a, b)
    }

    pub fn number_le(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_le(ctx, a, b)
    }

    pub fn number_ge(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_ge(ctx, a, b)
    }

    pub fn string2symbol(ctx: MutationContext<'gc>, s: Value<'gc>) -> Value<'gc> {
        misc::string2symbol(ctx, s)
    }

    pub fn symbol2string(ctx: MutationContext<'gc>, s: Value<'gc>) -> Value<'gc> {
        misc::symbol2string(ctx, s)
    }

    pub fn number_plus(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_plus(ctx, a, b)
    }

    pub fn number_minus(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_minus(ctx, a, b)
    }

    pub fn number_times(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_times(ctx, a, b)
    }

    pub fn number_div(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::number_div(ctx, a, b)
    }

    pub fn quotient(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::quotient(ctx, a, b)
    }

    pub fn remainder(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::remainder(ctx, a, b)
    }

    pub fn modulo(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::modulo(ctx, a, b)
    }

    pub fn exact2inexact(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::exact2inexact(ctx, n)
    }

    pub fn inexact_to_exact(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::inexact_to_exact(ctx, n)
    }

    pub fn expt(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::expt(ctx, a, b)
    }

    pub fn ash(ctx: MutationContext<'gc>, n: Value<'gc>, count: Value<'gc>) -> Value<'gc> {
        numbers::ash(ctx, n, count)
    }

    pub fn logand(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::logand(ctx, a, b)
    }

    pub fn logior(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        numbers::logior(ctx, a, b)
    }

    pub fn logxor(ctx: MutationContext<'gc>, a: Value<'gc>, b: Value<'gc>) -> Value<'gc> {
        misc::logxor(ctx, a, b)
    }

    pub fn lognot(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::lognot(ctx, n)
    }

    pub fn sqrt(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::sqrt(ctx, n)
    }

    pub fn abs(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::abs(ctx, n)
    }

    pub fn floor(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::floor(ctx, n)
    }

    pub fn ceiling(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::ceiling(ctx, n)
    }

    pub fn truncate(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::truncate(ctx, n)
    }

    pub fn sin(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::sin(ctx, n)
    }

    pub fn cos(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::cos(ctx, n)
    }

    pub fn tan(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::tan(ctx, n)
    }

    pub fn asin(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::asin(ctx, n)
    }

    pub fn acos(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::acos(ctx, n)
    }

    pub fn atan(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::atan(ctx, n)
    }

    pub fn exp(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::exp(ctx, n)
    }

    pub fn log(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        numbers::log(ctx, n)
    }

    pub fn atan2(ctx: MutationContext<'gc>, y: Value<'gc>, x: Value<'gc>) -> Value<'gc> {
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

    pub fn nanp(ctx: MutationContext<'gc>, v: Value<'gc>) -> Value<'gc> {
        preds::nanp(ctx, v)
    }

    pub fn integerp(ctx: MutationContext<'gc>, v: Value<'gc>) -> bool {
        preds::integerp(ctx, v)
    }

    pub fn exactp(ctx: MutationContext<'gc>, v: Value<'gc>) -> Value<'gc> {
        preds::exactp(ctx, v)
    }

    pub fn inexactp(ctx: MutationContext<'gc>, v: Value<'gc>) -> Value<'gc> {
        preds::inexactp(ctx, v)
    }

    pub fn evenp(ctx: MutationContext<'gc>, v: Value<'gc>) -> Value<'gc> {
        numbers::evenp(ctx, v)
    }

    pub fn oddp(ctx: MutationContext<'gc>, v: Value<'gc>) -> Value<'gc> {
        numbers::oddp(ctx, v)
    }

    pub fn exact_integerp(ctx: MutationContext<'gc>, v: Value<'gc>) -> Value<'gc> {
        preds::exact_integerp(ctx, v)
    }

    pub fn char_to_integer(ctx: MutationContext<'gc>, c: Value<'gc>) -> Value<'gc> {
        misc::char_to_integer(ctx, c)
    }

    pub fn integer_to_char(ctx: MutationContext<'gc>, n: Value<'gc>) -> Value<'gc> {
        misc::integer_to_char(ctx, n)
    }

    pub fn append(ctx: MutationContext<'gc>, m1: Value<'gc>, m2: Value<'gc>) -> Value<'gc> {
        pairs::append(ctx, m1, m2)
    }


    pub fn length(ctx: MutationContext<'gc>, v: Value<'gc>) -> Value<'gc> {
        pairs::length(ctx, v)
    }

    pub fn make_vector(ctx: MutationContext<'gc>, size: Value<'gc>, fill: Value<'gc>) -> Value<'gc> {
        vectors::make_vector(ctx, size, fill)
    }

    pub fn make_tuple(ctx: MutationContext<'gc>, size: Value<'gc>, fill: Value<'gc>) -> Value<'gc> {
        vectors::make_tuple(ctx, size, fill)
    }

    pub fn vector_ref(ctx: MutationContext<'gc>, vec: Value<'gc>, index: Value<'gc>) -> Value<'gc> {
        vectors::vector_ref(ctx, vec, index)
    }

    pub fn string_ref(ctx: MutationContext<'gc>, s: Value<'gc>, index: Value<'gc>) -> Value<'gc> {
        misc::string_ref_value(ctx, s, index)
    }

    pub fn bytevector_length(ctx: MutationContext<'gc>, bv: Value<'gc>) -> Value<'gc> {
        vectors::bytevector_length(ctx, bv)
    }

    pub fn bytevector_u8_ref(ctx: MutationContext<'gc>, bv: Value<'gc>, index: Value<'gc>) -> Value<'gc> {
        vectors::bytevector_u8_ref(ctx, bv, index)
    }

    pub fn vector_set(ctx: MutationContext<'gc>, vec: Value<'gc>, index: Value<'gc>, new_value: Value<'gc>) -> Value<'gc> {
        vectors::vector_set(ctx, vec, index, new_value)
    }

    pub fn tuple_size(ctx: MutationContext<'gc>, tup: Value<'gc>) -> Value<'gc> {
        vectors::tuple_size(ctx, tup)
    }

    pub fn current_continuation_marks(ctx: MutationContext<'gc>) -> Value<'gc> {
        misc::current_continuation_marks(ctx)
    }

    pub fn set_attachments(ctx: MutationContext<'gc>,
        marks: Value<'gc>) -> Value<'gc> {
        misc::set_attachments(ctx, marks)
    }

    pub fn attachment_push(ctx: MutationContext<'gc>, table: Value<'gc>) -> Value<'gc> {
        misc::attachment_push(ctx, table)
    }

    pub fn attachment_pop(ctx: MutationContext<'gc>) -> Value<'gc> {
        misc::attachment_pop(ctx)
    }

    pub fn attachment_set(ctx: MutationContext<'gc>, table: Value<'gc>) -> Value<'gc> {
        misc::attachment_set(ctx, table)
    }

    pub fn attachment_consume(ctx: MutationContext<'gc>) -> Value<'gc> {
        misc::attachment_consume(ctx)
    }

    pub fn attachment_consume_reify(ctx: MutationContext<'gc>, default: Value<'gc>) -> Value<'gc> {
        misc::attachment_consume_reify(ctx, default)
    }

    pub fn update_mark(ctx: MutationContext<'gc>, marks: Value<'gc>, key: Value<'gc>, val: Value<'gc>) -> Value<'gc> {
        misc::update_mark(ctx, marks, key, val)
    }

    pub fn make_syntax(ctx: MutationContext<'gc>,
        exp: Value<'gc>,
        wrap: Value<'gc>,
        module: Value<'gc>,
        source: Value<'gc>,
        properties: Value<'gc>) -> Value<'gc> {
        misc::make_syntax(ctx, exp, wrap, module, source, properties)
    }

    pub fn exception_handler(ctx: MutationContext<'gc>) -> Value<'gc> {
        control::exception_handler(ctx)
    }

    pub fn fxeq(ctx: MutationContext<'gc>,
        x: Value<'gc>,
        y: Value<'gc>) -> Value<'gc> {
        numbers::fxeq(ctx, x, y)
    }

    pub fn pre_write_barrier_at_slot(ctx: MutationContext<'gc>,
        src: ObjectReference,
        slot: ObjectSlot,
        target: ObjectReference) -> () {
        gc::pre_write_barrier_at_slot(ctx, src, slot, target)
    }

    pub fn post_write_barrier_at_slot(ctx: MutationContext<'gc>,
        src: ObjectReference,
        slot: ObjectSlot,
        target: ObjectReference) -> () {
        gc::post_write_barrier_at_slot(ctx, src, slot, target)
    }

    pub fn post_write_barrier_slow(ctx: MutationContext<'gc>,
        src: ObjectReference,
        offset: i32,
        target: ObjectReference) -> () {
        gc::post_write_barrier_slow(ctx, src, offset, target)
    }


    pub fn alloc_with_header_word(ctx: MutationContext<'gc>,
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
    ctx: MutationContext<'gc>,
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

    match crate::runtime::sni::call_function(ctx, assertion_violation, args) {
        ExecutionResult::Ok(val) => val,
        ExecutionResult::Err(err) => err,
    }
}

pub fn make_undefined_violation<'gc>(
    ctx: MutationContext<'gc>,
    who: Value<'gc>,
    message: impl AsRef<str>,
    irritants: &[Value<'gc>],
) -> Value<'gc> {
    let message: Value = Str::new(ctx, message, true).into();
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
    match crate::runtime::sni::call_function(ctx, undefined_violation, args) {
        ExecutionResult::Ok(val) => val,
        ExecutionResult::Err(err) => err,
    }
}

pub fn make_error<'gc>(
    ctx: MutationContext<'gc>,
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

    match crate::runtime::sni::call_function(ctx, error, args) {
        ExecutionResult::Ok(val) => val,
        ExecutionResult::Err(err) => err,
    }
}

pub fn make_io_error<'gc>(
    ctx: MutationContext<'gc>,
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
            // Pre-boot: the Scheme condition machinery is not loaded yet, so
            // build a minimal string error instead of panicking.
            Str::new(
                ctx,
                format!(
                    "i/o error (pre-boot): who={who}, message={message}, irritants={irritants:?}",
                ),
                true,
            )
            .into()
        });

    match crate::runtime::sni::call_function(ctx, io_error, args) {
        ExecutionResult::Ok(val) => val,
        ExecutionResult::Err(err) => err,
    }
}

pub fn make_lexical_violation<'gc>(
    ctx: MutationContext<'gc>,
    who: &str,
    message: impl AsRef<str>,
) -> Value<'gc> {
    let who = Symbol::from_str(ctx, who).into();
    let message: Value = Str::new(ctx, message, true).into();
    let args = vec![who, message];

    let lexical_violation = ctx
        .globals()
        .root_module()
        .get(ctx, Symbol::from_str(ctx, "%make-lexical-violation").into())
        .unwrap_or_else(|| panic!("failed to resolve %make-lexical-violation (pre-boot): who={who}, message={message}",));

    match crate::runtime::sni::call_function(ctx, lexical_violation, args) {
        ExecutionResult::Ok(val) => val,
        ExecutionResult::Err(err) => err,
    }
}

pub fn resolve_module<'gc>(
    ctx: MutationContext<'gc>,
    name: Value<'gc>,
    public: bool,
) -> Value<'gc> {
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
        module
            .public_interface
            .get()
            .expect("index in range")
            .into()
    } else {
        module.into()
    }
}
