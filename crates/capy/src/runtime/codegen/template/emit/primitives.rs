//! Primitives: `primitive0` through `primitive_n`.

use onlineasm::onlineasm;

use super::memory;
use crate::bytecode::def::{
    OpCallPrimitive0, OpCallPrimitive1, OpCallPrimitive2, OpCallPrimitive3,
};
use crate::runtime::codegen::gc::alloc::AllocSize;
use crate::runtime::codegen::gc::{GcCodegen, header_word};
use crate::runtime::codegen::masm::{Cmp, Masm};
use crate::runtime::codegen::template::{
    Operand, PrimSlowId, SlowPath, TemplateContext, ftmp, tmp,
};
use crate::runtime::prims::def::Primitive;
use crate::runtime::sabi::{self, builtin_class_ids, regs};
use crate::runtime::value::Value;

macro_rules! prim_op {
    ($name:ident) => {
        pub fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            context.control(masm, SlowPath::PrimN);
        }
    };
}

prim_op!(op_call_primitive_n);

pub fn op_call_primitive0<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    match context.operand(
        masm,
        OpCallPrimitive0::OPERAND_PRIMITIVE,
        OpCallPrimitive0::SIGNED_PRIMITIVE,
    ) {
        // The JIT (the only `Immediate` context) knows the prim id at compile
        // time and selects the inline directly, with no runtime dispatch.
        Operand::Immediate(id) => match id {
            PRIM_UNSPEC => prim0_unspec(masm, context),
            _ => context.prim_direct(masm, id, &[], OpCallPrimitive0::OPERAND_DST),
        },
        // The interpreter (the only `Register` context) calls every prim
        // through the indexed slow call, Guile `vm-engine` style: no id
        // compares, just `PRIM_WRAPPERS[id](args)`.
        Operand::Register(id) => {
            let slow = masm.label();
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Register(id),
                &[],
                OpCallPrimitive0::OPERAND_DST,
            );
        }
    }
}

// Hot prim ids, ordered by measured frequency: the INTERP_PERF set gave
// `heap-object?` (~72M: fibc, lexer, slot-loop), `variable?` (~24M:
// vector-kernel), `char->integer` (~12.7M: lexer) for `primitive1`, and `eqv?`
// (~92M: lexer), `quotient` (~6.5M: fft) for `primitive2`, the particles
// frame loop added `procedure?` (~1400/frame), `null?` (~400/frame) and
// `unspecified` (~1400/frame). The rest are total predicates inlined for
// breadth, the JIT selects by id with no dispatch cost either way, so
// order is documentation only.
const PRIM_HEAP_OBJECT: i64 = Primitive::IsHeapObject as u16 as i64;
const PRIM_VARIABLE: i64 = Primitive::IsVariable as u16 as i64;
const PRIM_CHAR_TO_INTEGER: i64 = Primitive::CharToInteger as u16 as i64;
const PRIM_EQV: i64 = Primitive::IsEqv as u16 as i64;
const PRIM_QUOTIENT: i64 = Primitive::Quotient as u16 as i64;
const PRIM_UNSPEC: i64 = Primitive::Unspec as u16 as i64;
const PRIM_PROCEDURE: i64 = Primitive::IsProcedure as u16 as i64;
const PRIM_NULL: i64 = Primitive::IsNull as u16 as i64;
const PRIM_BOOLEAN: i64 = Primitive::IsBoolean as u16 as i64;
const PRIM_BYTEVECTOR: i64 = Primitive::IsBytevector as u16 as i64;
const PRIM_CHAR: i64 = Primitive::IsChar as u16 as i64;
const PRIM_EOF: i64 = Primitive::IsEofObject as u16 as i64;
const PRIM_FIXNUM: i64 = Primitive::IsFixnum as u16 as i64;
const PRIM_FLONUM: i64 = Primitive::IsFlonum as u16 as i64;
const PRIM_IMMEDIATE: i64 = Primitive::IsImmediate as u16 as i64;
const PRIM_PAIR: i64 = Primitive::IsPair as u16 as i64;
const PRIM_STRING: i64 = Primitive::IsString as u16 as i64;
const PRIM_SYMBOL: i64 = Primitive::IsSymbol as u16 as i64;
const PRIM_TUPLE: i64 = Primitive::IsTuple as u16 as i64;
const PRIM_UNSPECIFIED: i64 = Primitive::IsUnspecified as u16 as i64;
const PRIM_VECTOR: i64 = Primitive::IsVector as u16 as i64;
const PRIM_NOT: i64 = Primitive::Not as u16 as i64;
const PRIM_EQ: i64 = Primitive::IsEq as u16 as i64;
const PRIM_FLOOR: i64 = Primitive::Floor as u16 as i64;
const PRIM_CEILING: i64 = Primitive::Ceiling as u16 as i64;
const PRIM_TRUNCATE: i64 = Primitive::Truncate as u16 as i64;
const PRIM_EXACT_TO_INEXACT: i64 = Primitive::ExactToInexact as u16 as i64;
const PRIM_INEXACT_TO_EXACT: i64 = Primitive::InexactToExact as u16 as i64;
const PRIM_FL_FLOOR: i64 = Primitive::FlFloor as u16 as i64;
const PRIM_FL_CEILING: i64 = Primitive::FlCeiling as u16 as i64;
const PRIM_FL_TRUNCATE: i64 = Primitive::FlTruncate as u16 as i64;
const PRIM_FL_ROUND: i64 = Primitive::FlRound as u16 as i64;
const PRIM_FL_ABS: i64 = Primitive::FlAbs as u16 as i64;
const PRIM_MAKE_VECTOR: i64 = Primitive::MakeVector as u16 as i64;
const PRIM_CAR: i64 = Primitive::Car as u16 as i64;
const PRIM_CDR: i64 = Primitive::Cdr as u16 as i64;
const PRIM_TUPLE_REF: i64 = Primitive::TupleRef as u16 as i64;
const PRIM_TUPLE_SET: i64 = Primitive::TupleSet as u16 as i64;
const PRIM_TUPLE_SIZE: i64 = Primitive::TupleSize as u16 as i64;
const PRIM_FX_EQ: i64 = Primitive::FxEq as u16 as i64;
const PRIM_FX_LT: i64 = Primitive::FxLt as u16 as i64;
const PRIM_FX_LE: i64 = Primitive::FxLe as u16 as i64;
const PRIM_FX_GT: i64 = Primitive::FxGt as u16 as i64;
const PRIM_FX_GE: i64 = Primitive::FxGe as u16 as i64;
const PRIM_FX_ZERO: i64 = Primitive::FxZero as u16 as i64;
const PRIM_FX_POSITIVE: i64 = Primitive::FxPositive as u16 as i64;
const PRIM_FX_NEGATIVE: i64 = Primitive::FxNegative as u16 as i64;
const PRIM_FX_ODD: i64 = Primitive::FxOdd as u16 as i64;
const PRIM_FX_EVEN: i64 = Primitive::FxEven as u16 as i64;
const PRIM_FX_MIN: i64 = Primitive::FxMin as u16 as i64;
const PRIM_FX_MAX: i64 = Primitive::FxMax as u16 as i64;
const PRIM_FX_ADD: i64 = Primitive::FxAdd as u16 as i64;
const PRIM_FX_SUB: i64 = Primitive::FxSub as u16 as i64;
const PRIM_FX_MUL: i64 = Primitive::FxMul as u16 as i64;
const PRIM_FX_AND: i64 = Primitive::FxAnd as u16 as i64;
const PRIM_FX_IOR: i64 = Primitive::FxIor as u16 as i64;
const PRIM_FX_XOR: i64 = Primitive::FxXor as u16 as i64;
const PRIM_FX_NOT: i64 = Primitive::FxNot as u16 as i64;
const PRIM_FX_ASHL: i64 = Primitive::FxAshl as u16 as i64;
const PRIM_FX_ASHR: i64 = Primitive::FxAshr as u16 as i64;
const PRIM_FL_ADD: i64 = Primitive::FlAdd as u16 as i64;
const PRIM_FL_SUB: i64 = Primitive::FlSub as u16 as i64;
const PRIM_FL_MUL: i64 = Primitive::FlMul as u16 as i64;
const PRIM_FL_DIV: i64 = Primitive::FlDiv as u16 as i64;
const PRIM_FL_LT: i64 = Primitive::FlLt as u16 as i64;
const PRIM_FL_LE: i64 = Primitive::FlLe as u16 as i64;
const PRIM_FL_GT: i64 = Primitive::FlGt as u16 as i64;
const PRIM_FL_GE: i64 = Primitive::FlGe as u16 as i64;
const PRIM_FL_EQ: i64 = Primitive::FlEq as u16 as i64;
const PRIM_FL_ZERO: i64 = Primitive::FlZero as u16 as i64;
const PRIM_FL_POSITIVE: i64 = Primitive::FlPositive as u16 as i64;
const PRIM_FL_NEGATIVE: i64 = Primitive::FlNegative as u16 as i64;
const PRIM_FL_NAN: i64 = Primitive::FlNan as u16 as i64;
const PRIM_FL_INFINITE: i64 = Primitive::FlInfinite as u16 as i64;
const PRIM_FL_FINITE: i64 = Primitive::FlFinite as u16 as i64;
const PRIM_FL_MIN: i64 = Primitive::FlMin as u16 as i64;
const PRIM_FL_MAX: i64 = Primitive::FlMax as u16 as i64;
const PRIM_FL_SQRT: i64 = Primitive::FlSqrt as u16 as i64;
const PRIM_FX_ADD_OVF: i64 = Primitive::FxAddOvf as u16 as i64;
const PRIM_FX_SUB_OVF: i64 = Primitive::FxSubOvf as u16 as i64;
const PRIM_FX_MUL_OVF: i64 = Primitive::FxMulOvf as u16 as i64;
const PRIM_IS_ZERO: i64 = Primitive::IsZero as u16 as i64;
const PRIM_IS_EVEN: i64 = Primitive::IsEven as u16 as i64;
const PRIM_IS_ODD: i64 = Primitive::IsOdd as u16 as i64;
const PRIM_IS_NUMBER: i64 = Primitive::IsNumber as u16 as i64;
const PRIM_IS_COMPLEX: i64 = Primitive::IsComplex as u16 as i64;
const PRIM_IS_REAL: i64 = Primitive::IsReal as u16 as i64;
const PRIM_IS_RATIONAL: i64 = Primitive::IsRational as u16 as i64;
const PRIM_IS_INTEGER: i64 = Primitive::IsInteger as u16 as i64;
const PRIM_IS_EXACT_INTEGER: i64 = Primitive::IsExactInteger as u16 as i64;
const PRIM_IS_EXACT: i64 = Primitive::IsExact as u16 as i64;
const PRIM_IS_INEXACT: i64 = Primitive::IsInexact as u16 as i64;
const PRIM_IS_NAN: i64 = Primitive::IsNan as u16 as i64;
const PRIM_USIZE_TO_VALUE: i64 = Primitive::UsizeToValue as u16 as i64;
const PRIM_REFPTR: i64 = Primitive::Refptr as u16 as i64;
const PRIM_LOGAND: i64 = Primitive::Logand as u16 as i64;
const PRIM_LOGIOR: i64 = Primitive::Logior as u16 as i64;
const PRIM_VECTOR_REF: i64 = Primitive::VectorRef as u16 as i64;
const PRIM_VECTOR_SET: i64 = Primitive::VectorSet as u16 as i64;
const PRIM_STRING_LENGTH: i64 = Primitive::StringLength as u16 as i64;
const PRIM_BYTEVECTOR_LENGTH: i64 = Primitive::BytevectorLength as u16 as i64;
const PRIM_BYTEVECTOR_U8_REF: i64 = Primitive::BytevectorU8Ref as u16 as i64;
const PRIM_INTEGER_TO_CHAR: i64 = Primitive::IntegerToChar as u16 as i64;

pub fn op_call_primitive1<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    match context.operand(
        masm,
        OpCallPrimitive1::OPERAND_PRIMITIVE,
        OpCallPrimitive1::SIGNED_PRIMITIVE,
    ) {
        // The JIT (the only `Immediate` context) knows the prim id at compile
        // time and selects the inline directly, with no runtime dispatch.
        Operand::Immediate(id) => match id {
            PRIM_HEAP_OBJECT => prim1_heap_object(masm, context),
            PRIM_PROCEDURE => prim1_procedure(masm, context),
            PRIM_NULL => prim1_null(masm, context),
            PRIM_VARIABLE => prim1_variable(masm, context),
            PRIM_CHAR_TO_INTEGER => prim1_char_to_integer(masm, context),
            PRIM_FLOOR => prim1_floor(masm, context),
            PRIM_INEXACT_TO_EXACT => prim1_inexact_to_exact(masm, context),
            PRIM_MAKE_VECTOR => prim1_make_vector(masm, context),
            PRIM_BOOLEAN => prim1_boolean(masm, context),
            PRIM_BYTEVECTOR => prim1_bytevector(masm, context),
            PRIM_CEILING => prim1_ceiling(masm, context),
            PRIM_CHAR => prim1_char(masm, context),
            PRIM_EOF => prim1_eof_object(masm, context),
            PRIM_EXACT_TO_INEXACT => prim1_exact_to_inexact(masm, context),
            PRIM_FIXNUM => prim1_fixnum(masm, context),
            PRIM_FLONUM => prim1_flonum(masm, context),
            PRIM_FL_ABS => prim1_fl_abs(masm, context),
            PRIM_FL_CEILING => prim1_fl_ceiling(masm, context),
            PRIM_FL_FLOOR => prim1_fl_floor(masm, context),
            PRIM_FL_ROUND => prim1_fl_round(masm, context),
            PRIM_FL_TRUNCATE => prim1_fl_truncate(masm, context),
            PRIM_IMMEDIATE => prim1_immediate(masm, context),
            PRIM_PAIR => prim1_pair(masm, context),
            PRIM_STRING => prim1_string(masm, context),
            PRIM_SYMBOL => prim1_symbol(masm, context),
            PRIM_TRUNCATE => prim1_truncate(masm, context),
            PRIM_TUPLE => prim1_tuple(masm, context),
            PRIM_UNSPECIFIED => prim1_unspecified(masm, context),
            PRIM_VECTOR => prim1_vector(masm, context),
            PRIM_NOT => prim1_not(masm, context),
            PRIM_CAR => prim1_car(masm, context),
            PRIM_CDR => prim1_cdr(masm, context),
            PRIM_TUPLE_SIZE => prim1_tuple_size(masm, context),
            PRIM_FX_ZERO => prim1_fx_zero(masm, context),
            PRIM_FX_POSITIVE => prim1_fx_positive(masm, context),
            PRIM_FX_NEGATIVE => prim1_fx_negative(masm, context),
            PRIM_FX_ODD => prim1_fx_odd(masm, context),
            PRIM_FX_EVEN => prim1_fx_even(masm, context),
            PRIM_IS_ZERO => prim1_is_zero(masm, context),
            PRIM_IS_ODD => prim1_is_odd(masm, context),
            PRIM_IS_EVEN => prim1_is_even(masm, context),
            PRIM_FX_NOT => prim1_fx_not(masm, context),
            PRIM_FL_ZERO => prim1_fl_zero(masm, context),
            PRIM_FL_POSITIVE => prim1_fl_positive(masm, context),
            PRIM_FL_NEGATIVE => prim1_fl_negative(masm, context),
            PRIM_FL_NAN => prim1_fl_nan(masm, context),
            PRIM_FL_INFINITE => prim1_fl_infinite(masm, context),
            PRIM_FL_FINITE => prim1_fl_finite(masm, context),
            PRIM_FL_SQRT => prim1_fl_sqrt(masm, context),
            PRIM_IS_NUMBER => prim1_is_number(masm, context),
            PRIM_IS_COMPLEX => prim1_is_number(masm, context),
            PRIM_IS_REAL => prim1_is_real(masm, context),
            PRIM_IS_RATIONAL => prim1_is_rational(masm, context),
            PRIM_IS_INTEGER => prim1_is_integer(masm, context),
            PRIM_IS_EXACT_INTEGER => prim1_is_exact_integer(masm, context),
            PRIM_IS_EXACT => prim1_is_exact(masm, context),
            PRIM_IS_INEXACT => prim1_is_inexact(masm, context),
            PRIM_IS_NAN => prim1_is_nan(masm, context),
            PRIM_USIZE_TO_VALUE => prim1_usize_to_value(masm, context),
            PRIM_STRING_LENGTH => prim1_string_length(masm, context),
            PRIM_BYTEVECTOR_LENGTH => prim1_bytevector_length(masm, context),
            PRIM_INTEGER_TO_CHAR => prim1_integer_to_char(masm, context),
            _ => context.prim_direct(
                masm,
                id,
                &[OpCallPrimitive1::OPERAND_A],
                OpCallPrimitive1::OPERAND_DST,
            ),
        },
        // As `primitive0`: every id takes the indexed slow call.
        Operand::Register(id) => {
            let slow = masm.label();
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Register(id),
                &[OpCallPrimitive1::OPERAND_A],
                OpCallPrimitive1::OPERAND_DST,
            );
        }
    }
}

pub fn op_call_primitive2<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    match context.operand(
        masm,
        OpCallPrimitive2::OPERAND_PRIMITIVE,
        OpCallPrimitive2::SIGNED_PRIMITIVE,
    ) {
        // The JIT (the only `Immediate` context) knows the prim id at compile
        // time and selects the inline directly, with no runtime dispatch.
        Operand::Immediate(id) => match id {
            PRIM_EQV => prim2_eqv(masm, context),
            PRIM_QUOTIENT => prim2_quotient(masm, context),
            PRIM_EQ => prim2_eq(masm, context),
            PRIM_MAKE_VECTOR => prim2_make_vector(masm, context),
            PRIM_TUPLE_REF => prim2_tuple_ref(masm, context),
            PRIM_FX_EQ => prim2_fx_eq(masm, context),
            PRIM_FX_LT => prim2_fx_lt(masm, context),
            PRIM_FX_LE => prim2_fx_le(masm, context),
            PRIM_FX_GT => prim2_fx_gt(masm, context),
            PRIM_FX_GE => prim2_fx_ge(masm, context),
            PRIM_FX_MIN => prim2_fx_min(masm, context),
            PRIM_FX_MAX => prim2_fx_max(masm, context),
            PRIM_FX_ADD_OVF => prim2_fx_add_ovf(masm, context),
            PRIM_FX_SUB_OVF => prim2_fx_sub_ovf(masm, context),
            PRIM_FX_MUL_OVF => prim2_fx_mul_ovf(masm, context),
            PRIM_FX_ADD => prim2_fx_add(masm, context),
            PRIM_FX_SUB => prim2_fx_sub(masm, context),
            PRIM_FX_MUL => prim2_fx_mul(masm, context),
            PRIM_FX_AND => prim2_fx_and(masm, context),
            PRIM_FX_IOR => prim2_fx_ior(masm, context),
            PRIM_FX_XOR => prim2_fx_xor(masm, context),
            PRIM_FX_ASHL => prim2_fx_ashl(masm, context),
            PRIM_FX_ASHR => prim2_fx_ashr(masm, context),
            PRIM_FL_ADD => prim2_fl_add(masm, context),
            PRIM_FL_SUB => prim2_fl_sub(masm, context),
            PRIM_FL_MUL => prim2_fl_mul(masm, context),
            PRIM_FL_DIV => prim2_fl_div(masm, context),
            PRIM_FL_LT => prim2_fl_lt(masm, context),
            PRIM_FL_LE => prim2_fl_le(masm, context),
            PRIM_FL_GT => prim2_fl_gt(masm, context),
            PRIM_FL_GE => prim2_fl_ge(masm, context),
            PRIM_FL_EQ => prim2_fl_eq(masm, context),
            PRIM_FL_MIN => prim2_fl_min(masm, context),
            PRIM_FL_MAX => prim2_fl_max(masm, context),
            PRIM_REFPTR => prim2_refptr(masm, context),
            PRIM_LOGAND => prim2_logand(masm, context),
            PRIM_LOGIOR => prim2_logior(masm, context),
            PRIM_VECTOR_REF => prim2_vector_ref(masm, context),
            PRIM_BYTEVECTOR_U8_REF => prim2_bytevector_u8_ref(masm, context),
            _ => {
                if !memory::emit_primitive2(masm, context, id) {
                    context.prim_direct(
                        masm,
                        id,
                        &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
                        OpCallPrimitive2::OPERAND_DST,
                    );
                }
            }
        },
        // As `primitive0`: every id takes the indexed slow call.
        Operand::Register(id) => {
            let slow = masm.label();
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Register(id),
                &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
                OpCallPrimitive2::OPERAND_DST,
            );
        }
    }
}

pub fn op_call_primitive3<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    // The JIT (the only `Immediate` context) knows the prim id at compile
    // time and selects the inline directly, with no runtime dispatch.
    match context.operand(
        masm,
        OpCallPrimitive3::OPERAND_PRIMITIVE,
        OpCallPrimitive3::SIGNED_PRIMITIVE,
    ) {
        Operand::Immediate(id) => match id {
            PRIM_TUPLE_SET => prim3_tuple_set(masm, context),
            PRIM_VECTOR_SET => prim3_vector_set(masm, context),
            _ => {
                if !memory::emit_primitive3(masm, context, id) {
                    context.prim_direct(
                        masm,
                        id,
                        &[
                            OpCallPrimitive3::OPERAND_A,
                            OpCallPrimitive3::OPERAND_B,
                            OpCallPrimitive3::OPERAND_C,
                        ],
                        OpCallPrimitive3::OPERAND_DST,
                    );
                }
            }
        },
        Operand::Register(id) => {
            let slow = masm.label();
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Register(id),
                &[
                    OpCallPrimitive3::OPERAND_A,
                    OpCallPrimitive3::OPERAND_B,
                    OpCallPrimitive3::OPERAND_C,
                ],
                OpCallPrimitive3::OPERAND_DST,
            );
        }
    }
}

/// `heap-object?`: a total predicate (`raw & NOT_CELL_MASK == 0`, excluding
/// the empty sentinel), so there is no slow path.
fn prim1_heap_object<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (no, done) = (masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_cell(tmp::T1, tmp::T0, no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
        jmp {done};
    );
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// `variable?`: a heap class test, total like the one above.
fn prim1_variable<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (no, done) = (masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::VARIABLE, tmp::T0, no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
        jmp {done};
    );
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// `char->integer`: unbox a char to its fixnum codepoint. The tag test plus
/// the scalar range (surrogates and above-max go slow) mirror `switch_char`,
/// anything else raises through the generic path.
fn prim1_char_to_integer<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (slow, is_char) = (masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    let (mask, tag) = (Value::CHAR_MASK, Value::CHAR_TAG);
    onlineasm!(*masm.assembler();
        andq {tmp::T0}, {tmp::T1}, {mask};
        bqneq {tmp::T0}, {tag}, {slow};
        move {tmp::T0}, {tmp::T1};
        urshiftq {tmp::T0}, 16;
        andq {tmp::T0}, {tmp::T0}, 0xffffffff;
        move {tmp::T2}, 0xd800;
        bqb {tmp::T0}, {tmp::T2}, {is_char};
        move {tmp::T2}, 0x10ffff;
        bqa {tmp::T0}, {tmp::T2}, {slow};
        move {tmp::T2}, 0xdfff;
        bqa {tmp::T0}, {tmp::T2}, {is_char};
        jmp {slow};
    );
    masm.bind(is_char);
    masm.box_int32(tmp::T0, tmp::T0);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_CHAR_TO_INTEGER as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `eqv?`: identical bits are equivalent for every type (the first line of
/// `Value::eqv`). Past that, both fixnums (canonical boxing, so differing
/// bits mean differing values) and both chars (neither a cell nor a number)
/// are never equivalent. Mixed numbers and heap objects go slow.
fn prim2_eqv<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (yes, no, done, maybe_fixnums, slow) = (
        masm.label(),
        masm.label(),
        masm.label(),
        masm.label(),
        masm.label(),
    );
    context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
    onlineasm!(*masm.assembler();
        bqeq {tmp::T1}, {tmp::T2}, {yes};
    );
    let (mask, tag) = (Value::CHAR_MASK, Value::CHAR_TAG);
    onlineasm!(*masm.assembler();
        move {tmp::T3}, {mask};
        andq {tmp::T0}, {tmp::T1}, {tmp::T3};
        bqneq {tmp::T0}, {tag}, {maybe_fixnums};
        andq {tmp::T0}, {tmp::T2}, {tmp::T3};
        bqeq {tmp::T0}, {tag}, {no};
    );
    masm.bind(maybe_fixnums);
    masm.check_fixnums(tmp::T1, tmp::T2, tmp::T0, slow);
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
        jmp {done};
    );
    masm.bind(yes);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_EQV as u16),
        &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
        OpCallPrimitive2::OPERAND_DST,
    );
}

/// `make-vector` of one argument: fixnum sizes allocate inline (see
/// [`emit_make_vector`]), anything else runs the thunk.
fn prim1_make_vector<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    emit_make_vector(
        masm,
        context,
        OpCallPrimitive1::OPERAND_A,
        None,
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `make-vector` of two arguments: the second is the fill.
fn prim2_make_vector<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    emit_make_vector(
        masm,
        context,
        OpCallPrimitive2::OPERAND_A,
        Some(OpCallPrimitive2::OPERAND_B),
        OpCallPrimitive2::OPERAND_DST,
    );
}

/// `make-vector` with a fixnum size (`Vector::new::<false>`): allocate,
/// store the length, and fill, with raw init stores (a fresh object needs
/// no barrier). `fill` is `None` for the 1-argument form, whose fill is
/// undefined. Non-fixnum sizes, negatives and `i32::MAX` (rejected by the
/// thunk) fall through to a `prim_slow` call, which runs the thunk.
fn emit_make_vector<C: TemplateContext>(
    masm: &mut Masm,
    context: &mut C,
    size: usize,
    fill: Option<usize>,
    dst: usize,
) {
    let gc = GcCodegen::new();
    let slow = masm.label();
    let (t0, t1, t2, t3) = (tmp::T0, tmp::T1, tmp::T2, tmp::T3);
    context.load_register(masm, size, t0);
    masm.branch_if_not_fixnum(t0, t1, slow);
    masm.unbox_int32(t0, t0);
    onlineasm!(*masm.assembler();
        bqeq {t0}, {i32::MAX as i64}, {slow};
    );
    let pending = gc.emit_alloc(
        masm,
        AllocSize::Words {
            count: t0,
            base: sabi::OBJECT_HEADER_BYTES + sabi::VECTOR_SIZE,
        },
        header_word(builtin_class_ids::VECTOR),
        t1,
        t2,
        t3,
    );
    let length_at = masm.assembler().mem(t1, sabi::VECTOR_LENGTH as i32);
    onlineasm!(*masm.assembler();
        storeq {length_at}, {t0};
        addp {t2}, {t1}, {sabi::VECTOR_DATA as i64};
    );
    match fill {
        Some(fill) => context.load_register(masm, fill, t3),
        None => onlineasm!(*masm.assembler(); move {t3}, {Value::undefined().bits() as i64};),
    }
    gc.emit_fill(masm, t2, t0, t3);
    context.store_register(masm, dst, t1, t0);
    context.next(masm);
    gc.emit_alloc_slow(masm, context, pending);
    let id = PrimSlowId::Immediate(Primitive::MakeVector as u16);
    match fill {
        Some(fill) => context.prim_slow(masm, slow, id, &[size, fill], dst),
        None => context.prim_slow(masm, slow, id, &[size], dst),
    }
}

/// `quotient` over fixnums: exact and truncating. A zero divisor raises and
/// `MIN / -1` is a bignum, so both go slow, as do all non-fixnums.
fn prim2_quotient<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (slow, div) = (masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
    masm.check_fixnums(tmp::T1, tmp::T2, tmp::T0, slow);
    masm.unbox_int32(tmp::T1, tmp::T1);
    masm.unbox_int32(tmp::T2, tmp::T2);
    onlineasm!(*masm.assembler();
        btqz {tmp::T2}, {slow};
        bqneq {tmp::T2}, -1, {div};
        bqeq {tmp::T1}, {i32::MIN as i64}, {slow};
    );
    masm.bind(div);
    // `divis` is the signed form (`divi` divides unsigned).
    onlineasm!(*masm.assembler();
        divis {tmp::T0}, {tmp::T1}, {tmp::T2};
    );
    masm.box_int32(tmp::T0, tmp::T0);
    context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_QUOTIENT as u16),
        &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
        OpCallPrimitive2::OPERAND_DST,
    );
}

/// `unspecified`: the undefined sentinel. Total, no slow path.
fn prim0_unspec<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_UNDEFINED};
    );
    context.store_register(masm, OpCallPrimitive0::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// Total heap-class predicates: `pair?`, `procedure?`, `string?`,
/// `symbol?`, `vector?`, `tuple?`. Each is `branch_not_heap_class` with a
/// boolean result, so there is no slow path.
macro_rules! pred_heap_class {
    ($name:ident, $class:expr) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let (no, done) = (masm.label(), masm.label());
            context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
            masm.branch_not_heap_class(tmp::T1, $class, tmp::T0, no);
            onlineasm!(*masm.assembler();
                move {tmp::T0}, {Value::VALUE_TRUE};
                jmp {done};
            );
            masm.bind(no);
            onlineasm!(*masm.assembler();
                move {tmp::T0}, {Value::VALUE_FALSE};
            );
            masm.bind(done);
            context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
        }
    };
}

pred_heap_class!(prim1_pair, builtin_class_ids::PAIR);
pred_heap_class!(prim1_procedure, builtin_class_ids::CLOSURE);
pred_heap_class!(prim1_string, builtin_class_ids::STRING);
pred_heap_class!(prim1_symbol, builtin_class_ids::SYMBOL);
pred_heap_class!(prim1_vector, builtin_class_ids::VECTOR);
pred_heap_class!(prim1_tuple, builtin_class_ids::TUPLE);

/// Total immediate-equality predicates: `null?`, `eof-object?`,
/// `unspecified?`. One compare against the singleton, no slow path.
macro_rules! pred_const {
    ($name:ident, $const:expr) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let (no, done) = (masm.label(), masm.label());
            context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
            onlineasm!(*masm.assembler();
                bqneq {tmp::T1}, {$const}, {no};
                move {tmp::T0}, {Value::VALUE_TRUE};
                jmp {done};
            );
            masm.bind(no);
            onlineasm!(*masm.assembler();
                move {tmp::T0}, {Value::VALUE_FALSE};
            );
            masm.bind(done);
            context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
        }
    };
}

pred_const!(prim1_null, Value::VALUE_NULL);
pred_const!(prim1_eof_object, Value::VALUE_EOF);
pred_const!(prim1_unspecified, Value::VALUE_UNDEFINED);

/// `boolean?`: `(raw & !1) == VALUE_FALSE`, matching `is_bool`. Total.
fn prim1_boolean<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (no, done) = (masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    onlineasm!(*masm.assembler();
        andq {tmp::T0}, {tmp::T1}, {-2};
        bqneq {tmp::T0}, {Value::VALUE_FALSE}, {no};
        move {tmp::T0}, {Value::VALUE_TRUE};
        jmp {done};
    );
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// `char?`: the tag test plus the scalar range, matching `is_char`
/// (surrogates and above-max are not chars). Total.
fn prim1_char<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (no, yes, done) = (masm.label(), masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    let (mask, tag) = (Value::CHAR_MASK, Value::CHAR_TAG);
    onlineasm!(*masm.assembler();
        andq {tmp::T0}, {tmp::T1}, {mask};
        bqneq {tmp::T0}, {tag}, {no};
        move {tmp::T0}, {tmp::T1};
        urshiftq {tmp::T0}, 16;
        andq {tmp::T0}, {tmp::T0}, 0xffffffff;
        move {tmp::T2}, 0xd800;
        bqb {tmp::T0}, {tmp::T2}, {yes};
        move {tmp::T2}, 0x10ffff;
        bqa {tmp::T0}, {tmp::T2}, {no};
        move {tmp::T2}, 0xdfff;
        bqa {tmp::T0}, {tmp::T2}, {yes};
        jmp {no};
    );
    masm.bind(yes);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
        jmp {done};
    );
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// `fixnum?` / `flonum?`: the single-compare tag tests. Total.
fn prim1_fixnum<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (no, done) = (masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_fixnum(tmp::T1, tmp::T0, no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
        jmp {done};
    );
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

fn prim1_flonum<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (no, done) = (masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_double(tmp::T1, tmp::T0, no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
        jmp {done};
    );
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// `immediate?`: `raw & NOT_CELL_MASK != 0`, matching the oracle exactly
/// (the empty sentinel is not immediate). Total.
fn prim1_immediate<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (no, done) = (masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_immediate(tmp::T1, tmp::T0, no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
        jmp {done};
    );
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// `bytevector?`: any of the three bytevector classes. Each alternative
/// reuses `branch_not_heap_class` with its exact empty/tag/header checks,
/// only the first is hot.
fn prim1_bytevector<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (immut, mapped, no, done) = (masm.label(), masm.label(), masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_not_heap_class(
        tmp::T1,
        builtin_class_ids::MUTABLE_BYTEVECTOR,
        tmp::T0,
        immut,
    );
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
        jmp {done};
    );
    masm.bind(immut);
    masm.branch_not_heap_class(
        tmp::T1,
        builtin_class_ids::IMMUTABLE_BYTEVECTOR,
        tmp::T0,
        mapped,
    );
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
        jmp {done};
    );
    masm.bind(mapped);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::MAPPED_BYTEVECTOR, tmp::T0, no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
        jmp {done};
    );
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// `not`: `raw == VALUE_FALSE`, matching `!truthy`. Total.
fn prim1_not<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    let no = Value::VALUE_FALSE;
    onlineasm!(*masm.assembler();
        cqeq {tmp::T0}, {tmp::T1}, {no};
        orq {tmp::T0}, {tmp::T0}, {no};
    );
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// `car`/`cdr`: the pair's words. Non-pairs raise through the slow call,
/// which packs the same `CarNotPair`/`CdrNotPair` error as the arm.
macro_rules! prim1_pair_accessor {
    ($name:ident, $id:expr, $off:expr) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
            masm.branch_not_heap_class(tmp::T1, builtin_class_ids::PAIR, tmp::T0, slow);
            let slot = masm.assembler().mem(tmp::T1, $off as i32);
            onlineasm!(*masm.assembler();
                loadq {tmp::T0}, {slot};
            );
            context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive1::OPERAND_A],
                OpCallPrimitive1::OPERAND_DST,
            );
        }
    };
}

prim1_pair_accessor!(prim1_car, PRIM_CAR, sabi::PAIR_CAR);
prim1_pair_accessor!(prim1_cdr, PRIM_CDR, sabi::PAIR_CDR);

/// `tuple-size`: the length word, boxed. Non-tuples raise through the slow
/// call, lengths above `i32::MAX` take it too
fn prim1_tuple_size<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::TUPLE, tmp::T0, slow);
    let len_at = masm.assembler().mem(tmp::T1, sabi::TUPLE_LENGTH as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
        move {tmp::T2}, {i32::MAX as i64};
        bqa {tmp::T0}, {tmp::T2}, {slow};
    );
    masm.box_int32(tmp::T0, tmp::T0);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_TUPLE_SIZE as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `eq?`: identical bits, matching the oracle. Total.
fn prim2_eq<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
    let no = Value::VALUE_FALSE;
    onlineasm!(*masm.assembler();
        cqeq {tmp::T0}, {tmp::T1}, {tmp::T2};
        orq {tmp::T0}, {tmp::T0}, {no};
    );
    context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// `tuple-ref`: exactly `emit_vector_ref_fast`'s shape over tuples (prims
/// carry no metadata, so there is no profiling): class, fixnum index, one
/// unsigned bounds check, indexed load. Slow inputs run the arm, which
/// reads the same raw slot.
fn prim2_tuple_ref<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::TUPLE, tmp::T0, slow);
    masm.branch_if_not_fixnum(tmp::T2, tmp::T0, slow);
    masm.unbox_int32(tmp::T2, tmp::T2);
    let len_at = masm.assembler().mem(tmp::T1, sabi::TUPLE_LENGTH as i32);
    // One unsigned compare: a negative index is huge unsigned, so it fails
    // against any valid length exactly like an over-long index does.
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
        bqaeq {tmp::T2}, {tmp::T0}, {slow};
    );
    let slot = masm
        .assembler()
        .mem_indexed(tmp::T1, tmp::T2, 8, sabi::TUPLE_DATA as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {slot};
    );
    context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_TUPLE_REF as u16),
        &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
        OpCallPrimitive2::OPERAND_DST,
    );
}

/// Fixnum binary predicates (`fx=?`, `fx<?`, ...): both fixnums compare
/// by `$op` (unboxed, so the 64-bit compare is exact), anything else takes
/// the slow call, which raises like the arm.
macro_rules! prim2_fx_cmp {
    ($name:ident, $id:expr, $op:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
            context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
            masm.check_fixnums(tmp::T1, tmp::T2, tmp::T0, slow);
            masm.unbox_int32(tmp::T1, tmp::T1);
            masm.unbox_int32(tmp::T2, tmp::T2);
            let no = Value::VALUE_FALSE;
            onlineasm!(*masm.assembler();
                $op {tmp::T0}, {tmp::T1}, {tmp::T2};
                orq {tmp::T0}, {tmp::T0}, {no};
            );
            context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
                OpCallPrimitive2::OPERAND_DST,
            );
        }
    };
}

prim2_fx_cmp!(prim2_fx_eq, PRIM_FX_EQ, cqeq);
prim2_fx_cmp!(prim2_fx_lt, PRIM_FX_LT, cqlt);
prim2_fx_cmp!(prim2_fx_le, PRIM_FX_LE, cqlteq);
prim2_fx_cmp!(prim2_fx_gt, PRIM_FX_GT, cqgt);
prim2_fx_cmp!(prim2_fx_ge, PRIM_FX_GE, cqgteq);

/// Fixnum unary tests (`fxzero?`, `fxpositive?`, ...): one unbox and a
/// comparison against zero. Non-fixnums take the slow call.
macro_rules! prim1_fx_test {
    ($name:ident, $id:expr, $op:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
            masm.branch_if_not_fixnum(tmp::T1, tmp::T0, slow);
            masm.unbox_int32(tmp::T1, tmp::T1);
            let no = Value::VALUE_FALSE;
            onlineasm!(*masm.assembler();
                move {tmp::T2}, 0;
                $op {tmp::T0}, {tmp::T1}, {tmp::T2};
                orq {tmp::T0}, {tmp::T0}, {no};
            );
            context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive1::OPERAND_A],
                OpCallPrimitive1::OPERAND_DST,
            );
        }
    };
}

prim1_fx_test!(prim1_fx_zero, PRIM_FX_ZERO, cqeq);
prim1_fx_test!(prim1_fx_positive, PRIM_FX_POSITIVE, cqgt);
prim1_fx_test!(prim1_fx_negative, PRIM_FX_NEGATIVE, cqlt);
prim1_fx_test!(prim1_is_zero, PRIM_IS_ZERO, cqeq);

/// Fixnum parity (`fxodd?`, `fxeven?`, ...): the low bit, tested after
/// the unbox (the boxed tag bit is not the value bit).
macro_rules! prim1_fx_parity {
    ($name:ident, $id:expr, $want:expr) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
            masm.branch_if_not_fixnum(tmp::T1, tmp::T0, slow);
            masm.unbox_int32(tmp::T1, tmp::T1);
            let no = Value::VALUE_FALSE;
            onlineasm!(*masm.assembler();
                andq {tmp::T2}, {tmp::T1}, 1;
                move {tmp::T1}, {$want};
                cqeq {tmp::T0}, {tmp::T2}, {tmp::T1};
                orq {tmp::T0}, {tmp::T0}, {no};
            );
            context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive1::OPERAND_A],
                OpCallPrimitive1::OPERAND_DST,
            );
        }
    };
}

prim1_fx_parity!(prim1_fx_odd, PRIM_FX_ODD, 1);
prim1_fx_parity!(prim1_fx_even, PRIM_FX_EVEN, 0);
prim1_fx_parity!(prim1_is_odd, PRIM_IS_ODD, 1);
prim1_fx_parity!(prim1_is_even, PRIM_IS_EVEN, 0);

/// `fxmin`/`fxmax`: the lesser/greater unboxed value, reboxed. Equal
/// inputs agree, so either side is correct.
macro_rules! prim2_fx_minmax {
    ($name:ident, $id:expr, $branch:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let (slow, pick_a, done) = (masm.label(), masm.label(), masm.label());
            context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
            context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
            masm.check_fixnums(tmp::T1, tmp::T2, tmp::T0, slow);
            masm.unbox_int32(tmp::T1, tmp::T1);
            masm.unbox_int32(tmp::T2, tmp::T2);
            onlineasm!(*masm.assembler();
                $branch {tmp::T1}, {tmp::T2}, {pick_a};
                move {tmp::T0}, {tmp::T2};
                jmp {done};
            );
            masm.bind(pick_a);
            onlineasm!(*masm.assembler();
                move {tmp::T0}, {tmp::T1};
            );
            masm.bind(done);
            masm.box_int32(tmp::T0, tmp::T0);
            context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
                OpCallPrimitive2::OPERAND_DST,
            );
        }
    };
}

prim2_fx_minmax!(prim2_fx_min, PRIM_FX_MIN, bqlt);
prim2_fx_minmax!(prim2_fx_max, PRIM_FX_MAX, bqgt);

/// Checked fixnum arithmetic (`fx+/overflow`, ...): the checked op boxes
/// on success, overflow answers `#f` inline, exactly like the arm's
/// `ovf` helper. Only non-fixnums take the slow call.
macro_rules! prim2_fx_ovf {
    ($name:ident, $id:expr, $checked:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let (slow, overflowed, done) = (masm.label(), masm.label(), masm.label());
            context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
            context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
            masm.check_fixnums(tmp::T1, tmp::T2, tmp::T0, slow);
            masm.$checked(tmp::T0, tmp::T1, tmp::T2, tmp::T3, overflowed);
            onlineasm!(*masm.assembler();
                jmp {done};
            );
            masm.bind(overflowed);
            onlineasm!(*masm.assembler();
                move {tmp::T0}, {Value::VALUE_FALSE};
            );
            masm.bind(done);
            context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
                OpCallPrimitive2::OPERAND_DST,
            );
        }
    };
}

prim2_fx_ovf!(prim2_fx_add_ovf, PRIM_FX_ADD_OVF, fixnum_add_checked);
prim2_fx_ovf!(prim2_fx_sub_ovf, PRIM_FX_SUB_OVF, fixnum_sub_checked);
prim2_fx_ovf!(prim2_fx_mul_ovf, PRIM_FX_MUL_OVF, fixnum_mul_checked);

/// Bitwise logic (`fxand`, `fxior`, `fxxor`): the result always fits,
/// so the unchecked op on the unboxed values followed by `box_int32`
/// is exact. Only non-fixnums take the slow call.
macro_rules! prim2_fx_wrap {
    ($name:ident, $id:expr, $op:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
            context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
            masm.check_fixnums(tmp::T1, tmp::T2, tmp::T0, slow);
            masm.unbox_int32(tmp::T1, tmp::T1);
            masm.unbox_int32(tmp::T2, tmp::T2);
            onlineasm!(*masm.assembler();
                $op {tmp::T0}, {tmp::T1}, {tmp::T2};
            );
            masm.box_int32(tmp::T0, tmp::T0);
            context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
                OpCallPrimitive2::OPERAND_DST,
            );
        }
    };
}

/// Overflow-raising `fx+`, `fx-`, `fx*`: the checked op jumps to the slow
/// call on signed 32-bit overflow, and the out-of-line prim raises the
/// implementation-restriction condition the arm raises.
macro_rules! prim2_fx_raising {
    ($name:ident, $id:expr, $checked:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
            context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
            masm.check_fixnums(tmp::T1, tmp::T2, tmp::T0, slow);
            masm.$checked(tmp::T0, tmp::T1, tmp::T2, tmp::T3, slow);
            context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
                OpCallPrimitive2::OPERAND_DST,
            );
        }
    };
}

prim2_fx_raising!(prim2_fx_add, PRIM_FX_ADD, fixnum_add_checked);
prim2_fx_raising!(prim2_fx_sub, PRIM_FX_SUB, fixnum_sub_checked);
prim2_fx_raising!(prim2_fx_mul, PRIM_FX_MUL, fixnum_mul_checked);
prim2_fx_wrap!(prim2_fx_and, PRIM_FX_AND, andq);
prim2_fx_wrap!(prim2_fx_ior, PRIM_FX_IOR, orq);
prim2_fx_wrap!(prim2_fx_xor, PRIM_FX_XOR, xorq);

/// `fxnot`: complement of the unboxed value, reboxed.
fn prim1_fx_not<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_fixnum(tmp::T1, tmp::T0, slow);
    masm.unbox_int32(tmp::T1, tmp::T1);
    onlineasm!(*masm.assembler();
        notq {tmp::T1};
    );
    masm.box_int32(tmp::T0, tmp::T1);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_FX_NOT as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

macro_rules! prim2_fx_shift {
    ($name:ident, $id:expr, $op:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
            context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
            masm.check_fixnums(tmp::T1, tmp::T2, tmp::T0, slow);
            masm.unbox_int32(tmp::T1, tmp::T1);
            masm.unbox_int32(tmp::T2, tmp::T2);
            onlineasm!(*masm.assembler();
                move {tmp::T0}, {tmp::T1};
                andq {tmp::T2}, {tmp::T2}, 31;
                $op {tmp::T0}, {tmp::T2};
            );
            masm.box_int32(tmp::T0, tmp::T0);
            context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
                OpCallPrimitive2::OPERAND_DST,
            );
        }
    };
}

/// `fxarithmetic-shift-left`: counts outside 0..32 and results that do
/// not round-trip through 32 bits take the slow call, which raises.
fn prim2_fx_ashl<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
    masm.check_fixnums(tmp::T1, tmp::T2, tmp::T0, slow);
    masm.unbox_int32(tmp::T1, tmp::T1);
    masm.unbox_int32(tmp::T2, tmp::T2);
    onlineasm!(*masm.assembler();
        bqlt {tmp::T2}, 0, {slow};
        bqgt {tmp::T2}, 31, {slow};
        move {tmp::T0}, {tmp::T1};
        lshiftq {tmp::T0}, {tmp::T2};
    );
    masm.box_int32(tmp::T3, tmp::T0);
    masm.unbox_int32(tmp::T3, tmp::T3);
    onlineasm!(*masm.assembler();
        bqneq {tmp::T0}, {tmp::T3}, {slow};
    );
    masm.box_int32(tmp::T0, tmp::T0);
    context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_FX_ASHL as u16),
        &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
        OpCallPrimitive2::OPERAND_DST,
    );
}
prim2_fx_shift!(prim2_fx_ashr, PRIM_FX_ASHR, rshiftq);

/// Flonum arithmetic (`fl+`, `fl-`, `fl*`, `fl/`): the flonum-only leaf
/// of `arith.rs`, minus profiling (prims carry no metadata). Division
/// by zero stays a flonum, so only non-flonums take the slow call.
macro_rules! prim2_fl_arith {
    ($name:ident, $id:expr, $flonum:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
            context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
            masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
            masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
            masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
            masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
            masm.$flonum(tmp::T0, ftmp::F0, ftmp::F1, ftmp::F2);
            context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
                OpCallPrimitive2::OPERAND_DST,
            );
        }
    };
}

prim2_fl_arith!(prim2_fl_add, PRIM_FL_ADD, flonum_add);
prim2_fl_arith!(prim2_fl_sub, PRIM_FL_SUB, flonum_sub);
prim2_fl_arith!(prim2_fl_mul, PRIM_FL_MUL, flonum_mul);
prim2_fl_arith!(prim2_fl_div, PRIM_FL_DIV, flonum_div);

/// Flonum comparisons: `flonum_cmp_bool` answers `#f` for NaN under
/// every condition, exactly like the arms' Rust operators.
macro_rules! prim2_fl_cmp {
    ($name:ident, $id:expr, $cond:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
            context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
            masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
            masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
            masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
            masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
            masm.flonum_cmp_bool(tmp::T0, ftmp::F0, ftmp::F1, Cmp::$cond);
            context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
                OpCallPrimitive2::OPERAND_DST,
            );
        }
    };
}

prim2_fl_cmp!(prim2_fl_lt, PRIM_FL_LT, Lt);
prim2_fl_cmp!(prim2_fl_le, PRIM_FL_LE, Le);
prim2_fl_cmp!(prim2_fl_gt, PRIM_FL_GT, Gt);
prim2_fl_cmp!(prim2_fl_ge, PRIM_FL_GE, Ge);
prim2_fl_cmp!(prim2_fl_eq, PRIM_FL_EQ, Eq);

/// `flzero?`/`flpositive?`/`flnegative?`: compare against a zeroed
/// `+0.0` (`-0.0 == +0.0`, NaN compares false, all matching Rust).
macro_rules! prim1_fl_cmp0 {
    ($name:ident, $id:expr, $cond:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
            masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
            masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
            onlineasm!(*masm.assembler();
                move {tmp::T3}, 0;
                fq2d {ftmp::F1}, {tmp::T3};
            );
            masm.flonum_cmp_bool(tmp::T0, ftmp::F0, ftmp::F1, Cmp::$cond);
            context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive1::OPERAND_A],
                OpCallPrimitive1::OPERAND_DST,
            );
        }
    };
}

prim1_fl_cmp0!(prim1_fl_zero, PRIM_FL_ZERO, Eq);
prim1_fl_cmp0!(prim1_fl_positive, PRIM_FL_POSITIVE, Gt);
prim1_fl_cmp0!(prim1_fl_negative, PRIM_FL_NEGATIVE, Lt);

/// `flnan?`: the unordered self-comparison, no constants needed.
fn prim1_fl_nan<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
    masm.flonum_nan_bool(tmp::T0, tmp::T1, ftmp::F0, tmp::T3);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_FL_NAN as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `flinfinite?`: `|x| == +inf`, the infinity bits materialized the
/// same way `unbox_double_fpr` materializes `+0.0` in reverse.
fn prim1_fl_infinite<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
    masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
    let inf = 0x7ff0_0000_0000_0000u64 as i64;
    onlineasm!(*masm.assembler();
        absd {ftmp::F2}, {ftmp::F0};
        move {tmp::T3}, {inf};
        fq2d {ftmp::F1}, {tmp::T3};
    );
    masm.flonum_cmp_bool(tmp::T0, ftmp::F2, ftmp::F1, Cmp::Eq);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_FL_INFINITE as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `flfinite?`: the exponent below all-ones, read off the unboxed bits.
/// No float compare.
fn prim1_fl_finite<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
    masm.flonum_finite_bool(tmp::T0, tmp::T1, tmp::T3);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_FL_FINITE as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `flmin`/`flmax`: unordered (NaN) takes the slow call, which answers
/// the canonical NaN. There is no pure unordered branch (`bdnequn`
/// also fires on ordered-not-equal), so each side self-compares.
/// Ordered ties (`-0.0` vs `+0.0`) pick by the sign bit, matching
/// `fl_min`/`fl_max`: `min` keeps the negative zero, `max` the
/// positive one.
macro_rules! prim2_fl_minmax {
    ($name:ident, $id:expr, $take_a:ident, $sign_takes_a:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let (slow, take_a, take_b, done) =
                (masm.label(), masm.label(), masm.label(), masm.label());
            context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
            context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
            masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
            masm.branch_if_not_double(tmp::T2, tmp::T3, slow);
            masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
            masm.unbox_double_fpr(ftmp::F1, tmp::T2, tmp::T3);
            onlineasm!(*masm.assembler();
                bdnequn {ftmp::F0}, {ftmp::F0}, {slow};
                bdnequn {ftmp::F1}, {ftmp::F1}, {slow};
                $take_a {ftmp::F0}, {ftmp::F1}, {take_a};
                $take_a {ftmp::F1}, {ftmp::F0}, {take_b};
                fd2q {tmp::T3}, {ftmp::F0};
                $sign_takes_a {tmp::T3}, 0, {take_a};
                jmp {take_b};
            );
            masm.bind(take_a);
            masm.box_double_fpr(tmp::T0, ftmp::F0);
            onlineasm!(*masm.assembler();
                jmp {done};
            );
            masm.bind(take_b);
            masm.box_double_fpr(tmp::T0, ftmp::F1);
            masm.bind(done);
            context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
                OpCallPrimitive2::OPERAND_DST,
            );
        }
    };
}

prim2_fl_minmax!(prim2_fl_min, PRIM_FL_MIN, bdlt, bqlt);
prim2_fl_minmax!(prim2_fl_max, PRIM_FL_MAX, bdgt, bqgteq);

/// Unary float leaves (`flabs`, `flfloor`, `flceiling`, `fltruncate`,
/// `flround`, `flsqrt`): unbox, hardware op, canonicalizing box.
macro_rules! prim1_fl_unary {
    ($name:ident, $id:expr, $flonum:ident) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
            masm.branch_if_not_double(tmp::T1, tmp::T3, slow);
            masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T3);
            masm.$flonum(tmp::T0, ftmp::F0, ftmp::F1);
            context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive1::OPERAND_A],
                OpCallPrimitive1::OPERAND_DST,
            );
        }
    };
}

prim1_fl_unary!(prim1_fl_sqrt, PRIM_FL_SQRT, flonum_sqrt);

/// `number?` (and `complex?`, which is the same test: every number is
/// complex): inline numbers answer `#t`, heap numbers match their class,
/// everything else answers `#f`. Total, so no slow path. The chained
/// class tests re-run the cell check on the cold heap path, the hot
/// inline-number path is two branches.
fn prim1_is_number<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (try_rational, try_complex, yes, no, done) = (
        masm.label(),
        masm.label(),
        masm.label(),
        masm.label(),
        masm.label(),
    );
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_fixnum(tmp::T1, tmp::T0, yes);
    masm.branch_if_double(tmp::T1, tmp::T0, yes);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::BIGINT, tmp::T0, try_rational);
    onlineasm!(*masm.assembler();
        jmp {yes};
    );
    masm.bind(try_rational);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::RATIONAL, tmp::T0, try_complex);
    onlineasm!(*masm.assembler();
        jmp {yes};
    );
    masm.bind(try_complex);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::COMPLEX, tmp::T0, no);
    onlineasm!(*masm.assembler();
        jmp {yes};
    );
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
        jmp {done};
    );
    masm.bind(yes);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// `real?`: `number?` minus the complex class. Total, no slow path.
fn prim1_is_real<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (try_rational, yes, no, done) = (masm.label(), masm.label(), masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_fixnum(tmp::T1, tmp::T0, yes);
    masm.branch_if_double(tmp::T1, tmp::T0, yes);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::BIGINT, tmp::T0, try_rational);
    onlineasm!(*masm.assembler();
        jmp {yes};
    );
    masm.bind(try_rational);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::RATIONAL, tmp::T0, no);
    onlineasm!(*masm.assembler();
        jmp {yes};
    );
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
        jmp {done};
    );
    masm.bind(yes);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// Predicates with constant answers on both inline types (`exact?`,
/// `inexact?`, `exact-integer?`): fixnums and flonums never reach the
/// wrapper, heap numbers and (for the raising two) non-numbers do.
macro_rules! prim1_fixfl_const {
    ($name:ident, $id:expr, $fix_yes:expr, $fl_yes:expr) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let (slow, answer, done) = (masm.label(), masm.label(), masm.label());
            context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
            masm.branch_if_fixnum(tmp::T1, tmp::T0, answer);
            masm.branch_if_not_double(tmp::T1, tmp::T0, slow);
            let fl = if $fl_yes {
                Value::VALUE_TRUE
            } else {
                Value::VALUE_FALSE
            };
            onlineasm!(*masm.assembler();
                move {tmp::T0}, {fl};
                jmp {done};
            );
            masm.bind(answer);
            let fix = if $fix_yes {
                Value::VALUE_TRUE
            } else {
                Value::VALUE_FALSE
            };
            onlineasm!(*masm.assembler();
                move {tmp::T0}, {fix};
            );
            masm.bind(done);
            context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive1::OPERAND_A],
                OpCallPrimitive1::OPERAND_DST,
            );
        }
    };
}

prim1_fixfl_const!(prim1_is_exact, PRIM_IS_EXACT, true, false);
prim1_fixfl_const!(prim1_is_inexact, PRIM_IS_INEXACT, false, true);
prim1_fixfl_const!(prim1_is_exact_integer, PRIM_IS_EXACT_INTEGER, true, false);

/// `rational?`: fixnums are rational, flonums are iff finite, heap
/// numbers go slow (bignums and rationals answer `#t` there, complex
/// `#f`). Non-numbers answer `#f` through the same slow call.
fn prim1_is_rational<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (slow, yes, done) = (masm.label(), masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_fixnum(tmp::T1, tmp::T0, yes);
    masm.branch_if_not_double(tmp::T1, tmp::T0, slow);
    masm.flonum_finite_bool(tmp::T0, tmp::T1, tmp::T3);
    onlineasm!(*masm.assembler();
        jmp {done};
    );
    masm.bind(yes);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_IS_RATIONAL as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `integer?`: fixnums answer `#t` inline, flonums (integral iff finite
/// with a `round` fixpoint, left slow) and heap numbers go slow.
fn prim1_is_integer<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (slow, done) = (masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_fixnum(tmp::T1, tmp::T0, slow);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_TRUE};
        jmp {done};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_IS_INTEGER as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `nan?`: fixnums answer `#f`, flonums self-compare, heap numbers go
/// slow, non-numbers raise through the same slow call.
fn prim1_is_nan<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (slow, no, done) = (masm.label(), masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_fixnum(tmp::T1, tmp::T0, no);
    masm.branch_if_not_double(tmp::T1, tmp::T0, slow);
    masm.flonum_nan_bool(tmp::T0, tmp::T1, ftmp::F0, tmp::T3);
    onlineasm!(*masm.assembler();
        jmp {done};
    );
    masm.bind(no);
    onlineasm!(*masm.assembler();
        move {tmp::T0}, {Value::VALUE_FALSE};
    );
    masm.bind(done);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_IS_NAN as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `%usize->value`: the low 32 bits retagged. Total, the arm checks
/// nothing either.
fn prim1_usize_to_value<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    let number_tag = regs::NUMBER_TAG;
    onlineasm!(*masm.assembler();
        andq {tmp::T0}, {tmp::T1}, 0xffff_ffff;
        orq {tmp::T0}, {tmp::T0}, {number_tag};
    );
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

/// `%refptr`: the unchecked raw read the arm does (`base + u32 offset`),
/// for compiler-generated address arithmetic. Total, the arm checks
/// nothing either.
fn prim2_refptr<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
    onlineasm!(*masm.assembler();
        andq {tmp::T2}, {tmp::T2}, 0xffff_ffff;
        addq {tmp::T0}, {tmp::T1}, {tmp::T2};
        loadq {tmp::T0}, [{tmp::T0}];
    );
    context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
}

prim2_fx_wrap!(prim2_logand, PRIM_LOGAND, andq);
prim2_fx_wrap!(prim2_logior, PRIM_LOGIOR, orq);

/// `vector-ref`: `prim2_tuple_ref`'s shape over vectors: class, fixnum
/// index, one unsigned bounds check, indexed load.
fn prim2_vector_ref<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::VECTOR, tmp::T0, slow);
    masm.branch_if_not_fixnum(tmp::T2, tmp::T0, slow);
    masm.unbox_int32(tmp::T2, tmp::T2);
    let len_at = masm.assembler().mem(tmp::T1, sabi::VECTOR_LENGTH as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
        bqaeq {tmp::T2}, {tmp::T0}, {slow};
    );
    let slot = masm
        .assembler()
        .mem_indexed(tmp::T1, tmp::T2, 8, sabi::VECTOR_DATA as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {slot};
    );
    context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_VECTOR_REF as u16),
        &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
        OpCallPrimitive2::OPERAND_DST,
    );
}

/// `vector-set!`: `prim3_tuple_set`'s shape over vectors: guards, the
/// inline store barrier, the store, `undefined`.
fn prim3_vector_set<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive3::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive3::OPERAND_B, tmp::T2);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::VECTOR, tmp::T0, slow);
    masm.branch_if_not_fixnum(tmp::T2, tmp::T0, slow);
    masm.unbox_int32(tmp::T2, tmp::T2);
    let len_at = masm.assembler().mem(tmp::T1, sabi::VECTOR_LENGTH as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
        bqaeq {tmp::T2}, {tmp::T0}, {slow};
    );
    let gc = GcCodegen::new();
    let barrier = gc.emit_store_barrier(masm, tmp::T1, tmp::T0, tmp::T3, &[tmp::T2]);
    context.load_register(masm, OpCallPrimitive3::OPERAND_C, tmp::T3);
    let slot = masm
        .assembler()
        .mem_indexed(tmp::T1, tmp::T2, 8, sabi::VECTOR_DATA as i32);
    onlineasm!(*masm.assembler();
        storeq {slot}, {tmp::T3};
        move {tmp::T0}, {Value::VALUE_UNDEFINED};
    );
    context.store_register(masm, OpCallPrimitive3::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    gc.emit_store_barrier_slow(masm, context, barrier);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_VECTOR_SET as u16),
        &[
            OpCallPrimitive3::OPERAND_A,
            OpCallPrimitive3::OPERAND_B,
            OpCallPrimitive3::OPERAND_C,
        ],
        OpCallPrimitive3::OPERAND_DST,
    );
}

/// `string-length`: the length word, boxed. The arm reads it unchecked,
/// the inline checks the class first and runs the same arm on anything
/// else, so behavior (including on non-strings) is identical.
fn prim1_string_length<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::STRING, tmp::T0, slow);
    let len_at = masm.assembler().mem(tmp::T1, sabi::STRING_LENGTH as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
    );
    masm.box_int32(tmp::T0, tmp::T0);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_STRING_LENGTH as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `bytevector-length`: `prim1_tuple_size`'s shape over both bytevector
/// classes (mapped bytevectors stay slow, like the arm's
/// `is_bytevector` gate). Huge lengths go slow for the bignum box.
fn prim1_bytevector_length<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (slow, immutable, done) = (masm.label(), masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_not_heap_class(
        tmp::T1,
        builtin_class_ids::MUTABLE_BYTEVECTOR,
        tmp::T0,
        immutable,
    );
    let len_at = masm
        .assembler()
        .mem(tmp::T1, sabi::BYTEVECTOR_LENGTH as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
        move {tmp::T2}, {i32::MAX as i64};
        bqa {tmp::T0}, {tmp::T2}, {slow};
        jmp {done};
    );
    masm.bind(immutable);
    masm.branch_not_heap_class(
        tmp::T1,
        builtin_class_ids::IMMUTABLE_BYTEVECTOR,
        tmp::T0,
        slow,
    );
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
        move {tmp::T2}, {i32::MAX as i64};
        bqa {tmp::T0}, {tmp::T2}, {slow};
    );
    masm.bind(done);
    masm.box_int32(tmp::T0, tmp::T0);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_BYTEVECTOR_LENGTH as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `bytevector-u8-ref`: class (either bytevector class), fixnum index,
/// one unsigned bounds check, zero-extending byte load, box.
fn prim2_bytevector_u8_ref<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (slow, immutable, load) = (masm.label(), masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive2::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive2::OPERAND_B, tmp::T2);
    masm.branch_not_heap_class(
        tmp::T1,
        builtin_class_ids::MUTABLE_BYTEVECTOR,
        tmp::T0,
        immutable,
    );
    onlineasm!(*masm.assembler();
        jmp {load};
    );
    masm.bind(immutable);
    masm.branch_not_heap_class(
        tmp::T1,
        builtin_class_ids::IMMUTABLE_BYTEVECTOR,
        tmp::T0,
        slow,
    );
    masm.bind(load);
    masm.branch_if_not_fixnum(tmp::T2, tmp::T0, slow);
    masm.unbox_int32(tmp::T2, tmp::T2);
    let len_at = masm
        .assembler()
        .mem(tmp::T1, sabi::BYTEVECTOR_LENGTH as i32);
    let contents_at = masm
        .assembler()
        .mem(tmp::T1, sabi::BYTEVECTOR_CONTENTS as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
        bqaeq {tmp::T2}, {tmp::T0}, {slow};
        loadq {tmp::T0}, {contents_at};
        addq {tmp::T0}, {tmp::T0}, {tmp::T2};
        loadb {tmp::T0}, [{tmp::T0}];
    );
    masm.box_int32(tmp::T0, tmp::T0);
    context.store_register(masm, OpCallPrimitive2::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_BYTEVECTOR_U8_REF as u16),
        &[OpCallPrimitive2::OPERAND_A, OpCallPrimitive2::OPERAND_B],
        OpCallPrimitive2::OPERAND_DST,
    );
}

/// `integer->char`: fixnums in char range (surrogates excluded, like
/// `char::from_u32`) box directly, everything else raises or boxes
/// through the slow call.
fn prim1_integer_to_char<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_fixnum(tmp::T1, tmp::T0, slow);
    masm.unbox_int32(tmp::T1, tmp::T1);
    onlineasm!(*masm.assembler();
        bqlt {tmp::T1}, 0, {slow};
        bqgt {tmp::T1}, 0x10ffff, {slow};
        subq {tmp::T0}, {tmp::T1}, 0xd800;
        bqb {tmp::T0}, 0x800, {slow};
        lshiftq {tmp::T1}, 16;
        orq {tmp::T1}, {tmp::T1}, {Value::CHAR_TAG};
        move {tmp::T0}, {tmp::T1};
    );
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_INTEGER_TO_CHAR as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `tuple-set!`: exactly `op_vector_set`'s shape over tuples (no
/// profiling): guards, the inline store barrier, the store, `undefined`.
/// The first `primitive3` inline.
fn prim3_tuple_set<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let slow = masm.label();
    context.load_register(masm, OpCallPrimitive3::OPERAND_A, tmp::T1);
    context.load_register(masm, OpCallPrimitive3::OPERAND_B, tmp::T2);
    masm.branch_not_heap_class(tmp::T1, builtin_class_ids::TUPLE, tmp::T0, slow);
    masm.branch_if_not_fixnum(tmp::T2, tmp::T0, slow);
    masm.unbox_int32(tmp::T2, tmp::T2);
    let len_at = masm.assembler().mem(tmp::T1, sabi::TUPLE_LENGTH as i32);
    onlineasm!(*masm.assembler();
        loadq {tmp::T0}, {len_at};
        bqaeq {tmp::T2}, {tmp::T0}, {slow};
    );
    let gc = GcCodegen::new();
    // The barrier needs two scratches (`T0`, `T3`), `src` loads after it.
    let barrier = gc.emit_store_barrier(masm, tmp::T1, tmp::T0, tmp::T3, &[tmp::T2]);
    context.load_register(masm, OpCallPrimitive3::OPERAND_C, tmp::T3);
    let slot = masm
        .assembler()
        .mem_indexed(tmp::T1, tmp::T2, 8, sabi::TUPLE_DATA as i32);
    onlineasm!(*masm.assembler();
        storeq {slot}, {tmp::T3};
        move {tmp::T0}, {Value::VALUE_UNDEFINED};
    );
    context.store_register(masm, OpCallPrimitive3::OPERAND_DST, tmp::T0, tmp::T1);
    context.next(masm);
    gc.emit_store_barrier_slow(masm, context, barrier);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_TUPLE_SET as u16),
        &[
            OpCallPrimitive3::OPERAND_A,
            OpCallPrimitive3::OPERAND_B,
            OpCallPrimitive3::OPERAND_C,
        ],
        OpCallPrimitive3::OPERAND_DST,
    );
}

/// `floor`/`ceiling`/`truncate`: fixnums are identity, flonums round
/// through hardware, heap numbers and non-numbers take the generic path.
macro_rules! prim1_round {
    ($name:ident, $round:ident, $id:expr) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let (flonum, slow, done) = (masm.label(), masm.label(), masm.label());
            context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
            masm.branch_if_not_fixnum(tmp::T1, tmp::T0, flonum);
            context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T1, tmp::T0);
            onlineasm!(*masm.assembler();
                jmp {done};
            );
            masm.bind(flonum);
            masm.branch_if_not_double(tmp::T1, tmp::T0, slow);
            masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T0);
            masm.$round(tmp::T0, ftmp::F0, ftmp::F1);
            context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
            masm.bind(done);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive1::OPERAND_A],
                OpCallPrimitive1::OPERAND_DST,
            );
        }
    };
}

prim1_round!(prim1_floor, flonum_floor, PRIM_FLOOR);
prim1_round!(prim1_ceiling, flonum_ceiling, PRIM_CEILING);
prim1_round!(prim1_truncate, flonum_truncate, PRIM_TRUNCATE);

/// `exact->inexact`: fixnums convert, flonums are identity, everything
/// else (heap numbers, non-numbers) takes the generic path.
fn prim1_exact_to_inexact<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (maybe_flonum, slow, done) = (masm.label(), masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_fixnum(tmp::T1, tmp::T0, maybe_flonum);
    masm.fixnum_to_double(ftmp::F0, tmp::T1);
    masm.box_double_fpr(tmp::T0, ftmp::F0);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    onlineasm!(*masm.assembler();
        jmp {done};
    );
    masm.bind(maybe_flonum);
    masm.branch_if_not_double(tmp::T1, tmp::T0, slow);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T1, tmp::T0);
    masm.bind(done);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_EXACT_TO_INEXACT as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `inexact->exact`: exact fixnums are identity, non-negative integral
/// in-range flonums convert, fractional, out-of-range, negative, and
/// non-numeric inputs take the generic path. Negatives stay slow for
/// bug-compatibility (see `checked_flonum_to_fixnum`).
fn prim1_inexact_to_exact<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
    let (maybe_flonum, slow, done) = (masm.label(), masm.label(), masm.label());
    context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
    masm.branch_if_not_fixnum(tmp::T1, tmp::T0, maybe_flonum);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T1, tmp::T0);
    onlineasm!(*masm.assembler();
        jmp {done};
    );
    masm.bind(maybe_flonum);
    masm.branch_if_not_double(tmp::T1, tmp::T0, slow);
    masm.checked_flonum_to_fixnum(tmp::T0, tmp::T1, ftmp::F0, ftmp::F1, tmp::T0, slow);
    context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
    masm.bind(done);
    context.next(masm);
    context.prim_slow(
        masm,
        slow,
        PrimSlowId::Immediate(PRIM_INEXACT_TO_EXACT as u16),
        &[OpCallPrimitive1::OPERAND_A],
        OpCallPrimitive1::OPERAND_DST,
    );
}

/// `flfloor`/`flceiling`/`fltruncate`/`flround`/`flabs`: flonums compute
/// through hardware, anything else raises through the generic path.
macro_rules! prim1_fl_unary {
    ($name:ident, $op:ident, $id:expr) => {
        fn $name<C: TemplateContext>(masm: &mut Masm, context: &mut C) {
            let slow = masm.label();
            context.load_register(masm, OpCallPrimitive1::OPERAND_A, tmp::T1);
            masm.branch_if_not_double(tmp::T1, tmp::T0, slow);
            masm.unbox_double_fpr(ftmp::F0, tmp::T1, tmp::T0);
            masm.$op(tmp::T0, ftmp::F0, ftmp::F1);
            context.store_register(masm, OpCallPrimitive1::OPERAND_DST, tmp::T0, tmp::T1);
            context.next(masm);
            context.prim_slow(
                masm,
                slow,
                PrimSlowId::Immediate($id as u16),
                &[OpCallPrimitive1::OPERAND_A],
                OpCallPrimitive1::OPERAND_DST,
            );
        }
    };
}

prim1_fl_unary!(prim1_fl_floor, flonum_floor, PRIM_FL_FLOOR);
prim1_fl_unary!(prim1_fl_ceiling, flonum_ceiling, PRIM_FL_CEILING);
prim1_fl_unary!(prim1_fl_truncate, flonum_truncate, PRIM_FL_TRUNCATE);
prim1_fl_unary!(prim1_fl_round, flonum_round, PRIM_FL_ROUND);
prim1_fl_unary!(prim1_fl_abs, flonum_abs, PRIM_FL_ABS);
