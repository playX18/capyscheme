pub mod arith;
pub mod lowlevel;
pub mod misc;
pub mod modules;
pub mod pairs;
pub mod preds;
pub mod unchecked;
pub mod vectors;

use super::SsaBuilder;
use crate::compiler::cps::graph::Atom;
use crate::runtime::{
    Context,
    value::{Symbol, Value},
};
use cranelift_codegen::ir;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primitive {
    // Low-level / memory
    ClassIdp,
    Refptr,
    UsizeToValue,
    CacheRef,
    CacheSet,
    IsImmediate,
    IsHeapObject,

    // Modules / variables
    VariableBound,
    VariableRef,
    VariableSet,
    MakeBox,
    EnsureLocalVar,
    Lookup,
    LookupBound,
    LookupBoundPublic,
    LookupBoundPrivate,
    CurrentModule,
    Define,
    IsVariable,

    // Pairs / lists
    SetCar,
    SetCdr,
    Cons,
    Reverse,
    IsEofObject,
    IsNull,
    Unspec,
    IsUnspecified,
    IsPair,
    IsList,
    Append,
    List,
    Memq,
    Memv,
    Car,
    Cdr,
    Length,

    // Vectors / tuples
    IsVector,
    IsBytevector,
    Vector,
    Tuple,
    VectorRef,
    VectorSet,
    BytevectorLength,
    BytevectorU8Ref,
    TupleSize,
    TupleRef,
    TupleSet,
    IsTuple,

    // Arithmetic / math
    FxEq,
    Ash,
    Logand,
    Logior,
    Lognot,
    Expt,
    Abs,
    Sqrt,
    Cos,
    Sin,
    Tan,
    Atan,
    Asin,
    Acos,
    Ceiling,
    Floor,
    Truncate,
    Plus,
    Minus,
    Times,
    Div,
    NumericEqual,
    NumericLt,
    NumericGt,
    NumericGte,
    NumericLte,
    ExactToInexact,
    InexactToExact,
    IsEven,
    IsOdd,
    IsZero,
    Quotient,
    Remainder,
    Modulo,

    // Predicates / equality
    IsProcedure,
    IsString,
    IsBoolean,
    IsSymbol,
    IsEq,
    IsEqv,
    IsEqual,
    IsExactInteger,
    IsInteger,
    IsChar,
    IsNumber,
    IsComplex,
    IsNan,
    IsReal,
    IsRational,
    IsInexact,
    IsExact,

    // Misc / control
    Not,
    MakeTuple,
    MakeVector,
    StringLength,
    StringRef,
    IntegerToChar,
    CharToInteger,
    Breakpoint,
    SymbolToString,
    StringToSymbol,
    PushCframe,
    CurrentContinuationMarks,
    SetAttachments,
    Winders,
    MakeSyntax,
    DefaultRetk,

    // SBBV: type predicates
    IsFixnum,
    IsFlonum,

    // SBBV: overflow-checked fixnum ops
    FxAddOvf,
    FxSubOvf,
    FxMulOvf,

    // SBBV: unchecked fixnum ops
    FxAdd,
    FxSub,
    FxMul,
    FxLt,
    FxLe,
    FxGt,
    FxGe,
    FxEqU,

    // SBBV: unchecked flonum ops
    FlAdd,
    FlSub,
    FlMul,
    FlDiv,
    FlLt,
    FlLe,
    FlGt,
    FlGe,
    FlEq,

    // SBBV: unchecked pair/vector/string ops
    CarUnchecked,
    CdrUnchecked,
    SetCarUnchecked,
    SetCdrUnchecked,
    VectorRefUnchecked,
    VectorSetUnchecked,
    VectorLengthUnchecked,
    CharToIntUnchecked,
    StringLengthUnchecked,
    StringRefUnchecked,

    // SBBV: unchecked fixnum division
    FxQuotient,
    FxRemainder,

    // SBBV: unchecked flonum unary math
    FlSqrt,
    FlAtan,

    // SBBV: unchecked bytevector ops
    BytevectorLengthUnchecked,
    BytevectorU8RefUnchecked,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum PrimValue {
    Value(ir::Value),
    Comparison(ir::Value),
}

pub struct PrimitiveLowerer<'gc> {
    pub map: HashMap<Value<'gc>, Primitive>,
}

impl Primitive {
    pub const ALL: &[Self] = &[
        Self::ClassIdp,
        Self::Refptr,
        Self::UsizeToValue,
        Self::CacheRef,
        Self::CacheSet,
        Self::IsImmediate,
        Self::IsHeapObject,
        Self::VariableBound,
        Self::VariableRef,
        Self::VariableSet,
        Self::MakeBox,
        Self::EnsureLocalVar,
        Self::Lookup,
        Self::LookupBound,
        Self::LookupBoundPublic,
        Self::LookupBoundPrivate,
        Self::CurrentModule,
        Self::Define,
        Self::IsVariable,
        Self::SetCar,
        Self::SetCdr,
        Self::Cons,
        Self::Reverse,
        Self::IsEofObject,
        Self::IsNull,
        Self::Unspec,
        Self::IsUnspecified,
        Self::IsPair,
        Self::IsList,
        Self::Append,
        Self::List,
        Self::Memq,
        Self::Memv,
        Self::Car,
        Self::Cdr,
        Self::Length,
        Self::IsVector,
        Self::IsBytevector,
        Self::Vector,
        Self::Tuple,
        Self::VectorRef,
        Self::VectorSet,
        Self::BytevectorLength,
        Self::BytevectorU8Ref,
        Self::TupleSize,
        Self::TupleRef,
        Self::TupleSet,
        Self::IsTuple,
        Self::FxEq,
        Self::Ash,
        Self::Logand,
        Self::Logior,
        Self::Lognot,
        Self::Expt,
        Self::Abs,
        Self::Sqrt,
        Self::Cos,
        Self::Sin,
        Self::Tan,
        Self::Atan,
        Self::Asin,
        Self::Acos,
        Self::Ceiling,
        Self::Floor,
        Self::Truncate,
        Self::Plus,
        Self::Minus,
        Self::Times,
        Self::Div,
        Self::NumericEqual,
        Self::NumericLt,
        Self::NumericGt,
        Self::NumericGte,
        Self::NumericLte,
        Self::ExactToInexact,
        Self::InexactToExact,
        Self::IsEven,
        Self::IsOdd,
        Self::IsZero,
        Self::Quotient,
        Self::Remainder,
        Self::Modulo,
        Self::IsProcedure,
        Self::IsString,
        Self::IsBoolean,
        Self::IsSymbol,
        Self::IsEq,
        Self::IsEqv,
        Self::IsEqual,
        Self::IsExactInteger,
        Self::IsInteger,
        Self::IsChar,
        Self::IsNumber,
        Self::IsComplex,
        Self::IsNan,
        Self::IsReal,
        Self::IsRational,
        Self::IsInexact,
        Self::IsExact,
        Self::Not,
        Self::MakeTuple,
        Self::MakeVector,
        Self::StringLength,
        Self::StringRef,
        Self::IntegerToChar,
        Self::CharToInteger,
        Self::Breakpoint,
        Self::SymbolToString,
        Self::StringToSymbol,
        Self::PushCframe,
        Self::CurrentContinuationMarks,
        Self::SetAttachments,
        Self::Winders,
        Self::MakeSyntax,
        Self::DefaultRetk,
        Self::IsFixnum,
        Self::IsFlonum,
        Self::FxAddOvf,
        Self::FxSubOvf,
        Self::FxMulOvf,
        Self::FxAdd,
        Self::FxSub,
        Self::FxMul,
        Self::FxLt,
        Self::FxLe,
        Self::FxGt,
        Self::FxGe,
        Self::FxEqU,
        Self::FlAdd,
        Self::FlSub,
        Self::FlMul,
        Self::FlDiv,
        Self::FlLt,
        Self::FlLe,
        Self::FlGt,
        Self::FlGe,
        Self::FlEq,
        Self::CarUnchecked,
        Self::CdrUnchecked,
        Self::SetCarUnchecked,
        Self::SetCdrUnchecked,
        Self::VectorRefUnchecked,
        Self::VectorSetUnchecked,
        Self::VectorLengthUnchecked,
        Self::CharToIntUnchecked,
        Self::StringLengthUnchecked,
        Self::StringRefUnchecked,
        Self::FxQuotient,
        Self::FxRemainder,
        Self::FlSqrt,
        Self::FlAtan,
        Self::BytevectorLengthUnchecked,
        Self::BytevectorU8RefUnchecked,
    ];

    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "%class-id?" => Some(Self::ClassIdp),
            "%refptr" => Some(Self::Refptr),
            "usize->value" => Some(Self::UsizeToValue),
            "cache-ref" => Some(Self::CacheRef),
            "cache-set!" => Some(Self::CacheSet),
            "immediate?" => Some(Self::IsImmediate),
            "heap-object?" => Some(Self::IsHeapObject),
            "variable-bound?" => Some(Self::VariableBound),
            "variable-ref" => Some(Self::VariableRef),
            "variable-set!" => Some(Self::VariableSet),
            "make-variable" => Some(Self::MakeBox),
            "module-ensure-local-variable!" => Some(Self::EnsureLocalVar),
            "lookup" => Some(Self::Lookup),
            "lookup-bound" => Some(Self::LookupBound),
            "lookup-bound-public" => Some(Self::LookupBoundPublic),
            "lookup-bound-private" => Some(Self::LookupBoundPrivate),
            "current-module" => Some(Self::CurrentModule),
            "define" => Some(Self::Define),
            "variable?" => Some(Self::IsVariable),
            "set-car!" => Some(Self::SetCar),
            "set-cdr!" => Some(Self::SetCdr),
            "cons" => Some(Self::Cons),
            "reverse" => Some(Self::Reverse),
            "eof-object?" => Some(Self::IsEofObject),
            "null?" => Some(Self::IsNull),
            "unspecified" => Some(Self::Unspec),
            "unspecified?" => Some(Self::IsUnspecified),
            "pair?" => Some(Self::IsPair),
            "list?" => Some(Self::IsList),
            "append" => Some(Self::Append),
            "list" => Some(Self::List),
            "memq" => Some(Self::Memq),
            "memv" => Some(Self::Memv),
            "car" => Some(Self::Car),
            "cdr" => Some(Self::Cdr),
            "length" => Some(Self::Length),
            "vector?" => Some(Self::IsVector),
            "bytevector?" => Some(Self::IsBytevector),
            "vector" => Some(Self::Vector),
            "tuple" => Some(Self::Tuple),
            "vector-ref" => Some(Self::VectorRef),
            "vector-set!" => Some(Self::VectorSet),
            "bytevector-length" => Some(Self::BytevectorLength),
            "bytevector-u8-ref" => Some(Self::BytevectorU8Ref),
            "tuple-size" => Some(Self::TupleSize),
            "tuple-ref" => Some(Self::TupleRef),
            "tuple-set!" => Some(Self::TupleSet),
            "tuple?" => Some(Self::IsTuple),
            "fx=?" => Some(Self::FxEq),
            "ash" => Some(Self::Ash),
            "logand" => Some(Self::Logand),
            "logior" => Some(Self::Logior),
            "lognot" => Some(Self::Lognot),
            "expt" => Some(Self::Expt),
            "abs" => Some(Self::Abs),
            "sqrt" => Some(Self::Sqrt),
            "cos" => Some(Self::Cos),
            "sin" => Some(Self::Sin),
            "tan" => Some(Self::Tan),
            "atan" => Some(Self::Atan),
            "asin" => Some(Self::Asin),
            "acos" => Some(Self::Acos),
            "ceiling" => Some(Self::Ceiling),
            "floor" => Some(Self::Floor),
            "truncate" => Some(Self::Truncate),
            "+" => Some(Self::Plus),
            "-" => Some(Self::Minus),
            "*" => Some(Self::Times),
            "/" => Some(Self::Div),
            "=" => Some(Self::NumericEqual),
            "<" => Some(Self::NumericLt),
            ">" => Some(Self::NumericGt),
            ">=" => Some(Self::NumericGte),
            "<=" => Some(Self::NumericLte),
            "exact->inexact" => Some(Self::ExactToInexact),
            "inexact->exact" => Some(Self::InexactToExact),
            "even?" => Some(Self::IsEven),
            "odd?" => Some(Self::IsOdd),
            "zero?" => Some(Self::IsZero),
            "quotient" => Some(Self::Quotient),
            "remainder" => Some(Self::Remainder),
            "modulo" => Some(Self::Modulo),
            "procedure?" => Some(Self::IsProcedure),
            "string?" => Some(Self::IsString),
            "boolean?" => Some(Self::IsBoolean),
            "symbol?" => Some(Self::IsSymbol),
            "eq?" => Some(Self::IsEq),
            "eqv?" => Some(Self::IsEqv),
            "equal?" => Some(Self::IsEqual),
            "exact-integer?" => Some(Self::IsExactInteger),
            "integer?" => Some(Self::IsInteger),
            "char?" => Some(Self::IsChar),
            "number?" => Some(Self::IsNumber),
            "complex?" => Some(Self::IsComplex),
            "nan?" => Some(Self::IsNan),
            "real?" => Some(Self::IsReal),
            "rational?" => Some(Self::IsRational),
            "inexact?" => Some(Self::IsInexact),
            "exact?" => Some(Self::IsExact),
            "not" => Some(Self::Not),
            "make-tuple" => Some(Self::MakeTuple),
            "make-vector" => Some(Self::MakeVector),
            "string-length" => Some(Self::StringLength),
            "string-ref" => Some(Self::StringRef),
            "integer->char" => Some(Self::IntegerToChar),
            "char->integer" => Some(Self::CharToInteger),
            ".breakpoint" => Some(Self::Breakpoint),
            "symbol->string" => Some(Self::SymbolToString),
            "string->symbol" => Some(Self::StringToSymbol),
            "push-cframe" => Some(Self::PushCframe),
            "current-continuation-marks" => Some(Self::CurrentContinuationMarks),
            "$set-attachments!" => Some(Self::SetAttachments),
            "$winders" => Some(Self::Winders),
            "make-syntax" => Some(Self::MakeSyntax),
            "#%default-retk" => Some(Self::DefaultRetk),
            "fixnum?" => Some(Self::IsFixnum),
            "flonum?" => Some(Self::IsFlonum),
            "fx+/ovf?" => Some(Self::FxAddOvf),
            "fx-/ovf?" => Some(Self::FxSubOvf),
            "fx*/ovf?" => Some(Self::FxMulOvf),
            "fx+/unchecked" => Some(Self::FxAdd),
            "fx-/unchecked" => Some(Self::FxSub),
            "fx*/unchecked" => Some(Self::FxMul),
            "fx</unchecked" => Some(Self::FxLt),
            "fx<=/unchecked" => Some(Self::FxLe),
            "fx>/unchecked" => Some(Self::FxGt),
            "fx>=/unchecked" => Some(Self::FxGe),
            "fx=/unchecked" => Some(Self::FxEqU),
            "fl+/unchecked" => Some(Self::FlAdd),
            "fl-/unchecked" => Some(Self::FlSub),
            "fl*/unchecked" => Some(Self::FlMul),
            "fl//unchecked" => Some(Self::FlDiv),
            "fl</unchecked" => Some(Self::FlLt),
            "fl<=/unchecked" => Some(Self::FlLe),
            "fl>/unchecked" => Some(Self::FlGt),
            "fl>=/unchecked" => Some(Self::FlGe),
            "fl=/unchecked" => Some(Self::FlEq),
            "car/unchecked" => Some(Self::CarUnchecked),
            "cdr/unchecked" => Some(Self::CdrUnchecked),
            "set-car!/unchecked" => Some(Self::SetCarUnchecked),
            "set-cdr!/unchecked" => Some(Self::SetCdrUnchecked),
            "vector-ref/unchecked" => Some(Self::VectorRefUnchecked),
            "vector-set!/unchecked" => Some(Self::VectorSetUnchecked),
            "vector-length/unchecked" => Some(Self::VectorLengthUnchecked),
            "char->integer/unchecked" => Some(Self::CharToIntUnchecked),
            "string-length/unchecked" => Some(Self::StringLengthUnchecked),
            "string-ref/unchecked" => Some(Self::StringRefUnchecked),
            "quotient/unchecked" => Some(Self::FxQuotient),
            "remainder/unchecked" => Some(Self::FxRemainder),
            "flsqrt/unchecked" => Some(Self::FlSqrt),
            "flatan/unchecked" => Some(Self::FlAtan),
            "bytevector-length/unchecked" => Some(Self::BytevectorLengthUnchecked),
            "bytevector-u8-ref/unchecked" => Some(Self::BytevectorU8RefUnchecked),
            _ => None,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::ClassIdp => "%class-id?",
            Self::Refptr => "%refptr",
            Self::UsizeToValue => "usize->value",
            Self::CacheRef => "cache-ref",
            Self::CacheSet => "cache-set!",
            Self::IsImmediate => "immediate?",
            Self::IsHeapObject => "heap-object?",
            Self::VariableBound => "variable-bound?",
            Self::VariableRef => "variable-ref",
            Self::VariableSet => "variable-set!",
            Self::MakeBox => "make-variable",
            Self::EnsureLocalVar => "module-ensure-local-variable!",
            Self::Lookup => "lookup",
            Self::LookupBound => "lookup-bound",
            Self::LookupBoundPublic => "lookup-bound-public",
            Self::LookupBoundPrivate => "lookup-bound-private",
            Self::CurrentModule => "current-module",
            Self::Define => "define",
            Self::IsVariable => "variable?",
            Self::SetCar => "set-car!",
            Self::SetCdr => "set-cdr!",
            Self::Cons => "cons",
            Self::Reverse => "reverse",
            Self::IsEofObject => "eof-object?",
            Self::IsNull => "null?",
            Self::Unspec => "unspecified",
            Self::IsUnspecified => "unspecified?",
            Self::IsPair => "pair?",
            Self::IsList => "list?",
            Self::Append => "append",
            Self::List => "list",
            Self::Memq => "memq",
            Self::Memv => "memv",
            Self::Car => "car",
            Self::Cdr => "cdr",
            Self::Length => "length",
            Self::IsVector => "vector?",
            Self::IsBytevector => "bytevector?",
            Self::Vector => "vector",
            Self::Tuple => "tuple",
            Self::VectorRef => "vector-ref",
            Self::VectorSet => "vector-set!",
            Self::BytevectorLength => "bytevector-length",
            Self::BytevectorU8Ref => "bytevector-u8-ref",
            Self::TupleSize => "tuple-size",
            Self::TupleRef => "tuple-ref",
            Self::TupleSet => "tuple-set!",
            Self::IsTuple => "tuple?",
            Self::FxEq => "fx=?",
            Self::Ash => "ash",
            Self::Logand => "logand",
            Self::Logior => "logior",
            Self::Lognot => "lognot",
            Self::Expt => "expt",
            Self::Abs => "abs",
            Self::Sqrt => "sqrt",
            Self::Cos => "cos",
            Self::Sin => "sin",
            Self::Tan => "tan",
            Self::Atan => "atan",
            Self::Asin => "asin",
            Self::Acos => "acos",
            Self::Ceiling => "ceiling",
            Self::Floor => "floor",
            Self::Truncate => "truncate",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Times => "*",
            Self::Div => "/",
            Self::NumericEqual => "=",
            Self::NumericLt => "<",
            Self::NumericGt => ">",
            Self::NumericGte => ">=",
            Self::NumericLte => "<=",
            Self::ExactToInexact => "exact->inexact",
            Self::InexactToExact => "inexact->exact",
            Self::IsEven => "even?",
            Self::IsOdd => "odd?",
            Self::IsZero => "zero?",
            Self::Quotient => "quotient",
            Self::Remainder => "remainder",
            Self::Modulo => "modulo",
            Self::IsProcedure => "procedure?",
            Self::IsString => "string?",
            Self::IsBoolean => "boolean?",
            Self::IsSymbol => "symbol?",
            Self::IsEq => "eq?",
            Self::IsEqv => "eqv?",
            Self::IsEqual => "equal?",
            Self::IsExactInteger => "exact-integer?",
            Self::IsInteger => "integer?",
            Self::IsChar => "char?",
            Self::IsNumber => "number?",
            Self::IsComplex => "complex?",
            Self::IsNan => "nan?",
            Self::IsReal => "real?",
            Self::IsRational => "rational?",
            Self::IsInexact => "inexact?",
            Self::IsExact => "exact?",
            Self::Not => "not",
            Self::MakeTuple => "make-tuple",
            Self::MakeVector => "make-vector",
            Self::StringLength => "string-length",
            Self::StringRef => "string-ref",
            Self::IntegerToChar => "integer->char",
            Self::CharToInteger => "char->integer",
            Self::Breakpoint => ".breakpoint",
            Self::SymbolToString => "symbol->string",
            Self::StringToSymbol => "string->symbol",
            Self::PushCframe => "push-cframe",
            Self::CurrentContinuationMarks => "current-continuation-marks",
            Self::SetAttachments => "$set-attachments!",
            Self::Winders => "$winders",
            Self::MakeSyntax => "make-syntax",
            Self::DefaultRetk => "#%default-retk",
            Self::IsFixnum => "fixnum?",
            Self::IsFlonum => "flonum?",
            Self::FxAddOvf => "fx+/ovf?",
            Self::FxSubOvf => "fx-/ovf?",
            Self::FxMulOvf => "fx*/ovf?",
            Self::FxAdd => "fx+/unchecked",
            Self::FxSub => "fx-/unchecked",
            Self::FxMul => "fx*/unchecked",
            Self::FxLt => "fx</unchecked",
            Self::FxLe => "fx<=/unchecked",
            Self::FxGt => "fx>/unchecked",
            Self::FxGe => "fx>=/unchecked",
            Self::FxEqU => "fx=/unchecked",
            Self::FlAdd => "fl+/unchecked",
            Self::FlSub => "fl-/unchecked",
            Self::FlMul => "fl*/unchecked",
            Self::FlDiv => "fl//unchecked",
            Self::FlLt => "fl</unchecked",
            Self::FlLe => "fl<=/unchecked",
            Self::FlGt => "fl>/unchecked",
            Self::FlGe => "fl>=/unchecked",
            Self::FlEq => "fl=/unchecked",
            Self::CarUnchecked => "car/unchecked",
            Self::CdrUnchecked => "cdr/unchecked",
            Self::SetCarUnchecked => "set-car!/unchecked",
            Self::SetCdrUnchecked => "set-cdr!/unchecked",
            Self::VectorRefUnchecked => "vector-ref/unchecked",
            Self::VectorSetUnchecked => "vector-set!/unchecked",
            Self::VectorLengthUnchecked => "vector-length/unchecked",
            Self::CharToIntUnchecked => "char->integer/unchecked",
            Self::StringLengthUnchecked => "string-length/unchecked",
            Self::StringRefUnchecked => "string-ref/unchecked",
            Self::FxQuotient => "quotient/unchecked",
            Self::FxRemainder => "remainder/unchecked",
            Self::FlSqrt => "flsqrt/unchecked",
            Self::FlAtan => "flatan/unchecked",
            Self::BytevectorLengthUnchecked => "bytevector-length/unchecked",
            Self::BytevectorU8RefUnchecked => "bytevector-u8-ref/unchecked",
        }
    }

    pub fn lower<'gc_, 'a, 'f>(
        self,
        ssa: &mut SsaBuilder<'gc_, 'a, 'f>,
        args: &[Atom<'gc_>],
        source: Value<'gc_>,
    ) -> PrimValue {
        match self {
            Self::ClassIdp => lowlevel::lower_class_idp(ssa, args, source),
            Self::Refptr => lowlevel::lower_refptr(ssa, args, source),
            Self::UsizeToValue => lowlevel::lower_usize_to_value(ssa, args, source),
            Self::CacheRef => lowlevel::lower_cache_ref(ssa, args, source),
            Self::CacheSet => lowlevel::lower_cache_set(ssa, args, source),
            Self::IsImmediate => lowlevel::lower_is_immediate(ssa, args, source),
            Self::IsHeapObject => lowlevel::lower_is_heap_object(ssa, args, source),
            Self::VariableBound => modules::lower_variable_bound(ssa, args, source),
            Self::VariableRef => modules::lower_variable_ref(ssa, args, source),
            Self::VariableSet => modules::lower_variable_set(ssa, args, source),
            Self::MakeBox => modules::lower_make_box(ssa, args, source),
            Self::EnsureLocalVar => modules::lower_ensure_local_var(ssa, args, source),
            Self::Lookup => modules::lower_lookup(ssa, args, source),
            Self::LookupBound => modules::lower_lookup_bound(ssa, args, source),
            Self::LookupBoundPublic => modules::lower_lookup_bound_public(ssa, args, source),
            Self::LookupBoundPrivate => modules::lower_lookup_bound_private(ssa, args, source),
            Self::CurrentModule => modules::lower_current_module(ssa, args, source),
            Self::Define => modules::lower_define(ssa, args, source),
            Self::Not => misc::lower_not(ssa, args, source),
            Self::IsProcedure => preds::lower_is_procedure(ssa, args, source),
            Self::IsVariable => modules::lower_is_variable(ssa, args, source),
            Self::SetCar => pairs::lower_set_car(ssa, args, source),
            Self::SetCdr => pairs::lower_set_cdr(ssa, args, source),
            Self::Cons => pairs::lower_cons(ssa, args, source),
            Self::Reverse => pairs::lower_reverse(ssa, args, source),
            Self::IsEofObject => pairs::lower_is_eof_object(ssa, args, source),
            Self::IsNull => pairs::lower_is_null(ssa, args, source),
            Self::Unspec => pairs::lower_unspec(ssa, args, source),
            Self::IsUnspecified => pairs::lower_is_unspecified(ssa, args, source),
            Self::IsPair => pairs::lower_is_pair(ssa, args, source),
            Self::IsList => pairs::lower_is_list(ssa, args, source),
            Self::Append => pairs::lower_append(ssa, args, source),
            Self::List => pairs::lower_list(ssa, args, source),
            Self::IsVector => vectors::lower_is_vector(ssa, args, source),
            Self::IsBytevector => vectors::lower_is_bytevector(ssa, args, source),
            Self::Vector => vectors::lower_vector(ssa, args, source),
            Self::Tuple => vectors::lower_tuple(ssa, args, source),
            Self::MakeTuple => misc::lower_make_tuple(ssa, args, source),
            Self::MakeVector => misc::lower_make_vector(ssa, args, source),
            Self::VectorRef => vectors::lower_vector_ref(ssa, args, source),
            Self::VectorSet => vectors::lower_vector_set(ssa, args, source),
            Self::BytevectorLength => vectors::lower_bytevector_length(ssa, args, source),
            Self::BytevectorU8Ref => vectors::lower_bytevector_u8_ref(ssa, args, source),
            Self::IsString => preds::lower_is_string(ssa, args, source),
            Self::StringLength => misc::lower_string_length(ssa, args, source),
            Self::StringRef => misc::lower_string_ref(ssa, args, source),
            Self::IsBoolean => preds::lower_is_boolean(ssa, args, source),
            Self::IsSymbol => preds::lower_is_symbol(ssa, args, source),
            Self::IsEq => preds::lower_is_eq(ssa, args, source),
            Self::FxEq => arith::lower_fx_eq(ssa, args, source),
            Self::IsEqv => preds::lower_is_eqv(ssa, args, source),
            Self::IsEqual => preds::lower_is_equal(ssa, args, source),
            Self::IsExactInteger => preds::lower_is_exact_integer(ssa, args, source),
            Self::IsInteger => preds::lower_is_integer(ssa, args, source),
            Self::IsChar => preds::lower_is_char(ssa, args, source),
            Self::IsNumber => preds::lower_is_number(ssa, args, source),
            Self::IsComplex => preds::lower_is_complex(ssa, args, source),
            Self::IsNan => preds::lower_is_nan(ssa, args, source),
            Self::Ash => arith::lower_ash(ssa, args, source),
            Self::Logand => arith::lower_logand(ssa, args, source),
            Self::Logior => arith::lower_logior(ssa, args, source),
            Self::Lognot => arith::lower_lognot(ssa, args, source),
            Self::IntegerToChar => misc::lower_integer_to_char(ssa, args, source),
            Self::CharToInteger => misc::lower_char_to_integer(ssa, args, source),
            Self::Expt => arith::lower_expt(ssa, args, source),
            Self::Abs => arith::lower_abs(ssa, args, source),
            Self::Sqrt => arith::lower_sqrt(ssa, args, source),
            Self::Cos => arith::lower_cos(ssa, args, source),
            Self::Sin => arith::lower_sin(ssa, args, source),
            Self::Tan => arith::lower_tan(ssa, args, source),
            Self::Atan => arith::lower_atan(ssa, args, source),
            Self::Asin => arith::lower_asin(ssa, args, source),
            Self::Acos => arith::lower_acos(ssa, args, source),
            Self::Ceiling => arith::lower_ceiling(ssa, args, source),
            Self::Floor => arith::lower_floor(ssa, args, source),
            Self::Truncate => arith::lower_truncate(ssa, args, source),
            Self::Plus => arith::lower_plus(ssa, args, source),
            Self::Minus => arith::lower_minus(ssa, args, source),
            Self::Times => arith::lower_times(ssa, args, source),
            Self::Div => arith::lower_div(ssa, args, source),
            Self::NumericEqual => arith::lower_numeric_equal(ssa, args, source),
            Self::NumericLt => arith::lower_numeric_lt(ssa, args, source),
            Self::NumericGt => arith::lower_numeric_gt(ssa, args, source),
            Self::NumericGte => arith::lower_numeric_gte(ssa, args, source),
            Self::NumericLte => arith::lower_numeric_lte(ssa, args, source),
            Self::Memq => pairs::lower_memq(ssa, args, source),
            Self::Memv => pairs::lower_memv(ssa, args, source),
            Self::ExactToInexact => arith::lower_exact_to_inexact(ssa, args, source),
            Self::InexactToExact => arith::lower_inexact_to_exact(ssa, args, source),
            Self::IsReal => preds::lower_is_real(ssa, args, source),
            Self::IsRational => preds::lower_is_rational(ssa, args, source),
            Self::IsInexact => preds::lower_is_inexact(ssa, args, source),
            Self::IsExact => preds::lower_is_exact(ssa, args, source),
            Self::IsEven => arith::lower_is_even(ssa, args, source),
            Self::IsOdd => arith::lower_is_odd(ssa, args, source),
            Self::IsZero => arith::lower_is_zero(ssa, args, source),
            Self::Car => pairs::lower_car(ssa, args, source),
            Self::Cdr => pairs::lower_cdr(ssa, args, source),
            Self::Breakpoint => misc::lower_breakpoint(ssa, args, source),
            Self::TupleSize => vectors::lower_tuple_size(ssa, args, source),
            Self::TupleRef => vectors::lower_tuple_ref(ssa, args, source),
            Self::TupleSet => vectors::lower_tuple_set(ssa, args, source),
            Self::IsTuple => vectors::lower_is_tuple(ssa, args, source),
            Self::SymbolToString => misc::lower_symbol_to_string(ssa, args, source),
            Self::StringToSymbol => misc::lower_string_to_symbol(ssa, args, source),
            Self::Length => pairs::lower_length(ssa, args, source),
            Self::Quotient => arith::lower_quotient(ssa, args, source),
            Self::Remainder => arith::lower_remainder(ssa, args, source),
            Self::Modulo => arith::lower_modulo(ssa, args, source),
            Self::PushCframe => misc::lower_push_cframe(ssa, args, source),
            Self::CurrentContinuationMarks => {
                misc::lower_current_continuation_marks(ssa, args, source)
            }
            Self::SetAttachments => misc::lower_set_attachments(ssa, args, source),
            Self::Winders => misc::lower_winders(ssa, args, source),
            Self::MakeSyntax => misc::lower_make_syntax(ssa, args, source),
            Self::DefaultRetk => misc::lower_default_retk(ssa, args, source),
            Self::IsFixnum => unchecked::lower_is_fixnum(ssa, args, source),
            Self::IsFlonum => unchecked::lower_is_flonum(ssa, args, source),
            Self::FxAddOvf => unchecked::lower_fx_add_ovf(ssa, args, source),
            Self::FxSubOvf => unchecked::lower_fx_sub_ovf(ssa, args, source),
            Self::FxMulOvf => unchecked::lower_fx_mul_ovf(ssa, args, source),
            Self::FxAdd => unchecked::lower_fx_add(ssa, args, source),
            Self::FxSub => unchecked::lower_fx_sub(ssa, args, source),
            Self::FxMul => unchecked::lower_fx_mul(ssa, args, source),
            Self::FxLt => unchecked::lower_fx_lt(ssa, args, source),
            Self::FxLe => unchecked::lower_fx_le(ssa, args, source),
            Self::FxGt => unchecked::lower_fx_gt(ssa, args, source),
            Self::FxGe => unchecked::lower_fx_ge(ssa, args, source),
            Self::FxEqU => unchecked::lower_fx_eq_unchecked(ssa, args, source),
            Self::FlAdd => unchecked::lower_fl_add(ssa, args, source),
            Self::FlSub => unchecked::lower_fl_sub(ssa, args, source),
            Self::FlMul => unchecked::lower_fl_mul(ssa, args, source),
            Self::FlDiv => unchecked::lower_fl_div(ssa, args, source),
            Self::FlLt => unchecked::lower_fl_lt(ssa, args, source),
            Self::FlLe => unchecked::lower_fl_le(ssa, args, source),
            Self::FlGt => unchecked::lower_fl_gt(ssa, args, source),
            Self::FlGe => unchecked::lower_fl_ge(ssa, args, source),
            Self::FlEq => unchecked::lower_fl_eq(ssa, args, source),
            Self::CarUnchecked => unchecked::lower_car_unchecked(ssa, args, source),
            Self::CdrUnchecked => unchecked::lower_cdr_unchecked(ssa, args, source),
            Self::SetCarUnchecked => unchecked::lower_set_car_unchecked(ssa, args, source),
            Self::SetCdrUnchecked => unchecked::lower_set_cdr_unchecked(ssa, args, source),
            Self::VectorRefUnchecked => unchecked::lower_vector_ref_unchecked(ssa, args, source),
            Self::VectorSetUnchecked => unchecked::lower_vector_set_unchecked(ssa, args, source),
            Self::VectorLengthUnchecked => {
                unchecked::lower_vector_length_unchecked(ssa, args, source)
            }
            Self::CharToIntUnchecked => unchecked::lower_char_to_int_unchecked(ssa, args, source),
            Self::StringLengthUnchecked => {
                unchecked::lower_string_length_unchecked(ssa, args, source)
            }
            Self::StringRefUnchecked => unchecked::lower_string_ref_unchecked(ssa, args, source),
            Self::FxQuotient => unchecked::lower_fx_quotient(ssa, args, source),
            Self::FxRemainder => unchecked::lower_fx_remainder(ssa, args, source),
            Self::FlSqrt => unchecked::lower_fl_sqrt(ssa, args, source),
            Self::FlAtan => unchecked::lower_fl_atan(ssa, args, source),
            Self::BytevectorLengthUnchecked => {
                unchecked::lower_bytevector_length_unchecked(ssa, args, source)
            }
            Self::BytevectorU8RefUnchecked => {
                unchecked::lower_bytevector_u8_ref_unchecked(ssa, args, source)
            }
        }
    }
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

impl<'gc> PrimitiveLowerer<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        let mut map = HashMap::new();
        map.insert(
            Symbol::from_str(ctx, "%class-id?").into(),
            Primitive::ClassIdp,
        );
        map.insert(Symbol::from_str(ctx, "%refptr").into(), Primitive::Refptr);
        map.insert(
            Symbol::from_str(ctx, "usize->value").into(),
            Primitive::UsizeToValue,
        );
        map.insert(
            Symbol::from_str(ctx, "cache-ref").into(),
            Primitive::CacheRef,
        );
        map.insert(
            Symbol::from_str(ctx, "cache-set!").into(),
            Primitive::CacheSet,
        );
        map.insert(
            Symbol::from_str(ctx, "immediate?").into(),
            Primitive::IsImmediate,
        );
        map.insert(
            Symbol::from_str(ctx, "heap-object?").into(),
            Primitive::IsHeapObject,
        );
        map.insert(
            Symbol::from_str(ctx, "variable-bound?").into(),
            Primitive::VariableBound,
        );
        map.insert(
            Symbol::from_str(ctx, "variable-ref").into(),
            Primitive::VariableRef,
        );
        map.insert(
            Symbol::from_str(ctx, "variable-set!").into(),
            Primitive::VariableSet,
        );
        map.insert(
            Symbol::from_str(ctx, "make-variable").into(),
            Primitive::MakeBox,
        );
        map.insert(
            Symbol::from_str(ctx, "module-ensure-local-variable!").into(),
            Primitive::EnsureLocalVar,
        );
        map.insert(Symbol::from_str(ctx, "lookup").into(), Primitive::Lookup);
        map.insert(
            Symbol::from_str(ctx, "lookup-bound").into(),
            Primitive::LookupBound,
        );
        map.insert(
            Symbol::from_str(ctx, "lookup-bound-public").into(),
            Primitive::LookupBoundPublic,
        );
        map.insert(
            Symbol::from_str(ctx, "lookup-bound-private").into(),
            Primitive::LookupBoundPrivate,
        );
        map.insert(
            Symbol::from_str(ctx, "current-module").into(),
            Primitive::CurrentModule,
        );
        map.insert(Symbol::from_str(ctx, "define").into(), Primitive::Define);
        map.insert(
            Symbol::from_str(ctx, "variable?").into(),
            Primitive::IsVariable,
        );
        map.insert(Symbol::from_str(ctx, "set-car!").into(), Primitive::SetCar);
        map.insert(Symbol::from_str(ctx, "set-cdr!").into(), Primitive::SetCdr);
        map.insert(Symbol::from_str(ctx, "cons").into(), Primitive::Cons);
        map.insert(Symbol::from_str(ctx, "reverse").into(), Primitive::Reverse);
        map.insert(
            Symbol::from_str(ctx, "eof-object?").into(),
            Primitive::IsEofObject,
        );
        map.insert(Symbol::from_str(ctx, "null?").into(), Primitive::IsNull);
        map.insert(
            Symbol::from_str(ctx, "unspecified").into(),
            Primitive::Unspec,
        );
        map.insert(
            Symbol::from_str(ctx, "unspecified?").into(),
            Primitive::IsUnspecified,
        );
        map.insert(Symbol::from_str(ctx, "pair?").into(), Primitive::IsPair);
        map.insert(Symbol::from_str(ctx, "list?").into(), Primitive::IsList);
        map.insert(Symbol::from_str(ctx, "append").into(), Primitive::Append);
        map.insert(Symbol::from_str(ctx, "list").into(), Primitive::List);
        map.insert(Symbol::from_str(ctx, "memq").into(), Primitive::Memq);
        map.insert(Symbol::from_str(ctx, "memv").into(), Primitive::Memv);
        map.insert(Symbol::from_str(ctx, "car").into(), Primitive::Car);
        map.insert(Symbol::from_str(ctx, "cdr").into(), Primitive::Cdr);
        map.insert(Symbol::from_str(ctx, "length").into(), Primitive::Length);
        map.insert(Symbol::from_str(ctx, "vector?").into(), Primitive::IsVector);
        map.insert(
            Symbol::from_str(ctx, "bytevector?").into(),
            Primitive::IsBytevector,
        );
        map.insert(Symbol::from_str(ctx, "vector").into(), Primitive::Vector);
        map.insert(Symbol::from_str(ctx, "tuple").into(), Primitive::Tuple);
        map.insert(
            Symbol::from_str(ctx, "vector-ref").into(),
            Primitive::VectorRef,
        );
        map.insert(
            Symbol::from_str(ctx, "vector-set!").into(),
            Primitive::VectorSet,
        );
        map.insert(
            Symbol::from_str(ctx, "bytevector-length").into(),
            Primitive::BytevectorLength,
        );
        map.insert(
            Symbol::from_str(ctx, "bytevector-u8-ref").into(),
            Primitive::BytevectorU8Ref,
        );
        map.insert(
            Symbol::from_str(ctx, "tuple-size").into(),
            Primitive::TupleSize,
        );
        map.insert(
            Symbol::from_str(ctx, "tuple-ref").into(),
            Primitive::TupleRef,
        );
        map.insert(
            Symbol::from_str(ctx, "tuple-set!").into(),
            Primitive::TupleSet,
        );
        map.insert(Symbol::from_str(ctx, "tuple?").into(), Primitive::IsTuple);
        map.insert(Symbol::from_str(ctx, "fx=?").into(), Primitive::FxEq);
        map.insert(Symbol::from_str(ctx, "ash").into(), Primitive::Ash);
        map.insert(Symbol::from_str(ctx, "logand").into(), Primitive::Logand);
        map.insert(Symbol::from_str(ctx, "logior").into(), Primitive::Logior);
        map.insert(Symbol::from_str(ctx, "lognot").into(), Primitive::Lognot);
        map.insert(Symbol::from_str(ctx, "expt").into(), Primitive::Expt);
        map.insert(Symbol::from_str(ctx, "abs").into(), Primitive::Abs);
        map.insert(Symbol::from_str(ctx, "sqrt").into(), Primitive::Sqrt);
        map.insert(Symbol::from_str(ctx, "cos").into(), Primitive::Cos);
        map.insert(Symbol::from_str(ctx, "sin").into(), Primitive::Sin);
        map.insert(Symbol::from_str(ctx, "tan").into(), Primitive::Tan);
        map.insert(Symbol::from_str(ctx, "atan").into(), Primitive::Atan);
        map.insert(Symbol::from_str(ctx, "asin").into(), Primitive::Asin);
        map.insert(Symbol::from_str(ctx, "acos").into(), Primitive::Acos);
        map.insert(Symbol::from_str(ctx, "ceiling").into(), Primitive::Ceiling);
        map.insert(Symbol::from_str(ctx, "floor").into(), Primitive::Floor);
        map.insert(
            Symbol::from_str(ctx, "truncate").into(),
            Primitive::Truncate,
        );
        map.insert(Symbol::from_str(ctx, "+").into(), Primitive::Plus);
        map.insert(Symbol::from_str(ctx, "-").into(), Primitive::Minus);
        map.insert(Symbol::from_str(ctx, "*").into(), Primitive::Times);
        map.insert(Symbol::from_str(ctx, "/").into(), Primitive::Div);
        map.insert(Symbol::from_str(ctx, "=").into(), Primitive::NumericEqual);
        map.insert(Symbol::from_str(ctx, "<").into(), Primitive::NumericLt);
        map.insert(Symbol::from_str(ctx, ">").into(), Primitive::NumericGt);
        map.insert(Symbol::from_str(ctx, ">=").into(), Primitive::NumericGte);
        map.insert(Symbol::from_str(ctx, "<=").into(), Primitive::NumericLte);
        map.insert(
            Symbol::from_str(ctx, "exact->inexact").into(),
            Primitive::ExactToInexact,
        );
        map.insert(
            Symbol::from_str(ctx, "inexact->exact").into(),
            Primitive::InexactToExact,
        );
        map.insert(Symbol::from_str(ctx, "even?").into(), Primitive::IsEven);
        map.insert(Symbol::from_str(ctx, "odd?").into(), Primitive::IsOdd);
        map.insert(Symbol::from_str(ctx, "zero?").into(), Primitive::IsZero);
        map.insert(
            Symbol::from_str(ctx, "quotient").into(),
            Primitive::Quotient,
        );
        map.insert(
            Symbol::from_str(ctx, "remainder").into(),
            Primitive::Remainder,
        );
        map.insert(Symbol::from_str(ctx, "modulo").into(), Primitive::Modulo);
        map.insert(
            Symbol::from_str(ctx, "procedure?").into(),
            Primitive::IsProcedure,
        );
        map.insert(Symbol::from_str(ctx, "string?").into(), Primitive::IsString);
        map.insert(
            Symbol::from_str(ctx, "boolean?").into(),
            Primitive::IsBoolean,
        );
        map.insert(Symbol::from_str(ctx, "symbol?").into(), Primitive::IsSymbol);
        map.insert(Symbol::from_str(ctx, "eq?").into(), Primitive::IsEq);
        map.insert(Symbol::from_str(ctx, "eqv?").into(), Primitive::IsEqv);
        map.insert(Symbol::from_str(ctx, "equal?").into(), Primitive::IsEqual);
        map.insert(
            Symbol::from_str(ctx, "exact-integer?").into(),
            Primitive::IsExactInteger,
        );
        map.insert(
            Symbol::from_str(ctx, "integer?").into(),
            Primitive::IsInteger,
        );
        map.insert(Symbol::from_str(ctx, "char?").into(), Primitive::IsChar);
        map.insert(Symbol::from_str(ctx, "number?").into(), Primitive::IsNumber);
        map.insert(
            Symbol::from_str(ctx, "complex?").into(),
            Primitive::IsComplex,
        );
        map.insert(Symbol::from_str(ctx, "nan?").into(), Primitive::IsNan);
        map.insert(Symbol::from_str(ctx, "real?").into(), Primitive::IsReal);
        map.insert(
            Symbol::from_str(ctx, "rational?").into(),
            Primitive::IsRational,
        );
        map.insert(
            Symbol::from_str(ctx, "inexact?").into(),
            Primitive::IsInexact,
        );
        map.insert(Symbol::from_str(ctx, "exact?").into(), Primitive::IsExact);
        map.insert(Symbol::from_str(ctx, "not").into(), Primitive::Not);
        map.insert(
            Symbol::from_str(ctx, "make-tuple").into(),
            Primitive::MakeTuple,
        );
        map.insert(
            Symbol::from_str(ctx, "make-vector").into(),
            Primitive::MakeVector,
        );
        map.insert(
            Symbol::from_str(ctx, "string-length").into(),
            Primitive::StringLength,
        );
        map.insert(
            Symbol::from_str(ctx, "string-ref").into(),
            Primitive::StringRef,
        );
        map.insert(
            Symbol::from_str(ctx, "integer->char").into(),
            Primitive::IntegerToChar,
        );
        map.insert(
            Symbol::from_str(ctx, "char->integer").into(),
            Primitive::CharToInteger,
        );
        map.insert(
            Symbol::from_str(ctx, ".breakpoint").into(),
            Primitive::Breakpoint,
        );
        map.insert(
            Symbol::from_str(ctx, "symbol->string").into(),
            Primitive::SymbolToString,
        );
        map.insert(
            Symbol::from_str(ctx, "string->symbol").into(),
            Primitive::StringToSymbol,
        );
        map.insert(
            Symbol::from_str(ctx, "push-cframe").into(),
            Primitive::PushCframe,
        );
        map.insert(
            Symbol::from_str(ctx, "current-continuation-marks").into(),
            Primitive::CurrentContinuationMarks,
        );
        map.insert(
            Symbol::from_str(ctx, "$set-attachments!").into(),
            Primitive::SetAttachments,
        );
        map.insert(Symbol::from_str(ctx, "$winders").into(), Primitive::Winders);
        map.insert(
            Symbol::from_str(ctx, "make-syntax").into(),
            Primitive::MakeSyntax,
        );
        map.insert(
            Symbol::from_str(ctx, "#%default-retk").into(),
            Primitive::DefaultRetk,
        );
        map.insert(Symbol::from_str(ctx, "fixnum?").into(), Primitive::IsFixnum);
        map.insert(Symbol::from_str(ctx, "flonum?").into(), Primitive::IsFlonum);
        map.insert(
            Symbol::from_str(ctx, "fx+/ovf?").into(),
            Primitive::FxAddOvf,
        );
        map.insert(
            Symbol::from_str(ctx, "fx-/ovf?").into(),
            Primitive::FxSubOvf,
        );
        map.insert(
            Symbol::from_str(ctx, "fx*/ovf?").into(),
            Primitive::FxMulOvf,
        );
        map.insert(
            Symbol::from_str(ctx, "fx+/unchecked").into(),
            Primitive::FxAdd,
        );
        map.insert(
            Symbol::from_str(ctx, "fx-/unchecked").into(),
            Primitive::FxSub,
        );
        map.insert(
            Symbol::from_str(ctx, "fx*/unchecked").into(),
            Primitive::FxMul,
        );
        map.insert(
            Symbol::from_str(ctx, "fx</unchecked").into(),
            Primitive::FxLt,
        );
        map.insert(
            Symbol::from_str(ctx, "fx<=/unchecked").into(),
            Primitive::FxLe,
        );
        map.insert(
            Symbol::from_str(ctx, "fx>/unchecked").into(),
            Primitive::FxGt,
        );
        map.insert(
            Symbol::from_str(ctx, "fx>=/unchecked").into(),
            Primitive::FxGe,
        );
        map.insert(
            Symbol::from_str(ctx, "fx=/unchecked").into(),
            Primitive::FxEqU,
        );
        map.insert(
            Symbol::from_str(ctx, "fl+/unchecked").into(),
            Primitive::FlAdd,
        );
        map.insert(
            Symbol::from_str(ctx, "fl-/unchecked").into(),
            Primitive::FlSub,
        );
        map.insert(
            Symbol::from_str(ctx, "fl*/unchecked").into(),
            Primitive::FlMul,
        );
        map.insert(
            Symbol::from_str(ctx, "fl//unchecked").into(),
            Primitive::FlDiv,
        );
        map.insert(
            Symbol::from_str(ctx, "fl</unchecked").into(),
            Primitive::FlLt,
        );
        map.insert(
            Symbol::from_str(ctx, "fl<=/unchecked").into(),
            Primitive::FlLe,
        );
        map.insert(
            Symbol::from_str(ctx, "fl>/unchecked").into(),
            Primitive::FlGt,
        );
        map.insert(
            Symbol::from_str(ctx, "fl>=/unchecked").into(),
            Primitive::FlGe,
        );
        map.insert(
            Symbol::from_str(ctx, "fl=/unchecked").into(),
            Primitive::FlEq,
        );
        map.insert(
            Symbol::from_str(ctx, "car/unchecked").into(),
            Primitive::CarUnchecked,
        );
        map.insert(
            Symbol::from_str(ctx, "cdr/unchecked").into(),
            Primitive::CdrUnchecked,
        );
        map.insert(
            Symbol::from_str(ctx, "set-car!/unchecked").into(),
            Primitive::SetCarUnchecked,
        );
        map.insert(
            Symbol::from_str(ctx, "set-cdr!/unchecked").into(),
            Primitive::SetCdrUnchecked,
        );
        map.insert(
            Symbol::from_str(ctx, "vector-ref/unchecked").into(),
            Primitive::VectorRefUnchecked,
        );
        map.insert(
            Symbol::from_str(ctx, "vector-set!/unchecked").into(),
            Primitive::VectorSetUnchecked,
        );
        map.insert(
            Symbol::from_str(ctx, "vector-length/unchecked").into(),
            Primitive::VectorLengthUnchecked,
        );
        map.insert(
            Symbol::from_str(ctx, "char->integer/unchecked").into(),
            Primitive::CharToIntUnchecked,
        );
        map.insert(
            Symbol::from_str(ctx, "string-length/unchecked").into(),
            Primitive::StringLengthUnchecked,
        );
        map.insert(
            Symbol::from_str(ctx, "string-ref/unchecked").into(),
            Primitive::StringRefUnchecked,
        );
        map.insert(
            Symbol::from_str(ctx, "quotient/unchecked").into(),
            Primitive::FxQuotient,
        );
        map.insert(
            Symbol::from_str(ctx, "remainder/unchecked").into(),
            Primitive::FxRemainder,
        );
        map.insert(
            Symbol::from_str(ctx, "flsqrt/unchecked").into(),
            Primitive::FlSqrt,
        );
        map.insert(
            Symbol::from_str(ctx, "flatan/unchecked").into(),
            Primitive::FlAtan,
        );
        map.insert(
            Symbol::from_str(ctx, "bytevector-length/unchecked").into(),
            Primitive::BytevectorLengthUnchecked,
        );
        map.insert(
            Symbol::from_str(ctx, "bytevector-u8-ref/unchecked").into(),
            Primitive::BytevectorU8RefUnchecked,
        );
        Self { map }
    }

    pub fn primitive(&self, value: Value<'gc>) -> Option<Primitive> {
        self.map.get(&value).copied()
    }
}
