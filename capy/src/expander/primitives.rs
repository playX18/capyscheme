use crate::expander::core::{Fix, Let, Proc, Term, TermKind, TermRef, call_term, prim_call_term};
use crate::runtime::Context;
use crate::{runtime::value::Value, static_symbols};
use rsgc::alloc::array::Array;
use rsgc::{Gc, Trace};
use rsgc::{Rootable, global::Global};
use std::collections::HashMap;
use std::sync::OnceLock;

#[derive(Trace)]
#[collect(no_drop)]
pub struct Primitive<'gc> {
    pub name_str: &'static str,
    pub name: Value<'gc>,
    pub impure: bool,
}

impl<'gc> Primitive<'gc> {
    pub const fn new(name_str: &'static str, name: Value<'gc>, impure: bool) -> Self {
        Primitive {
            name_str,
            name,
            impure,
        }
    }

    pub const fn is_impure(&self) -> bool {
        self.impure
    }
}

#[derive(Trace)]
#[collect(no_drop)]
pub struct Primitives<'gc> {
    pub set: HashMap<Value<'gc>, Primitive<'gc>>,
}

macro_rules! make_primitives {
    ($(($name: ident, $l: literal, $impure: literal)),*) => {
        static_symbols!(
            $(
                $name = $l
            )*
        );

        pub static PRIMITIVES: OnceLock<Global<Rootable!(Primitives<'_>)>> = OnceLock::new();


        pub fn primitives<'gc>(ctx: Context<'gc>) -> &'gc Primitives<'gc> {
            &*PRIMITIVES.get_or_init(|| {
                let mut set = HashMap::new();

                $(
                    let name = paste::paste! {
                        [<$name:lower>](ctx)
                    };
                    let primitive = Primitive::new($l, name.into(), $impure);

                    set.insert(name.into(), primitive);
                )*

                Global::new(Primitives {

                    set
                })
            }).fetch(&ctx)
        }
    };
}

make_primitives!(
    (PRIM_DEFINE, "define", true),
    (PRIM_GSET, "gset!", true),
    (PRIM_BOXREF, "box-ref", true),
    (PRIM_BOXSET, "set-box!", true),
    (PRIM_BOXREF_UNCHECKED, "unsafe-box-ref", false),
    (PRIM_BOXSET_UNCHECKED, "unsafe-set-box!", true),
    (PRIM_BOX, "box", false),
    (PRIM_BOXP, "box?", false),
    (PRIM_SET_CAR, "set-car!", true),
    (PRIM_SET_CDR, "set-cdr!", true),
    (PRIM_CAR, "car", true),
    (PRIM_CDR, "cdr", true),
    (PRIM_CADR, "cadr", true),
    (PRIM_CDDR, "cddr", true),
    (PRIM_CDAR, "cdar", true),
    (PRIM_CAAR, "caar", true),
    (PRIM_CAAAR, "caaar", true),
    (PRIM_CADAR, "cadar", true),
    (PRIM_CDDAR, "cddar", true),
    (PRIM_CDDDR, "cdddr", true),
    (PRIM_CADDR, "caddr", true),
    (PRIM_CDDDDR, "cddddr", true),
    (PRIM_CONS, "cons", true),
    (PRIM_CAR_UNCHECKED, "unsafe-car", true),
    (PRIM_CDR_UNCHECKED, "unsafe-cdr", true),
    (PRIM_NULLP, "null?", false),
    (PRIM_PAIRP, "pair?", false),
    (PRIM_APPEND, "append", true),
    (PRIM_LIST, "list", false),
    (PRIM_LISTP, "list?", false),
    (PRIM_VECTOR, "vector", false),
    (PRIM_VECTORP, "vector?", false),
    (PRIM_VECTOR_REF, "vector-ref", true),
    (PRIM_VECTOR_SET, "vector-set!", true),
    (PRIM_VECTOR_REF_UNCHECKED, "unsafe-vector-ref", true),
    (PRIM_VECTOR_SET_UNCHECKED, "unsafe-vector-set!", true),
    (PRIM_STRING, "string", false),
    (PRIM_STRINGP, "string?", false),
    (PRIM_STRING_LENGTH, "string-length", true),
    (PRIM_STRING_REF, "string-ref", true),
    (PRIM_STRING_SET, "string-set!", true),
    (PRIM_STRING_REF_UNCHECKED, "unsafe-string-ref", false),
    (PRIM_STRING_SET_UNCHECKED, "unsafe-string-set!", false),
    (PRIM_STRING_LENGTH_UNCHECKED, "unsafe-string-length", false),
    (PRIM_STRING_TO_SYMBOL, "string->symbol", true),
    (PRIM_SYMBOL_TO_STRING, "symbol->string", true),
    (PRIM_NOT, "not", false),
    (PRIM_BOOLEANP, "boolean?", false),
    (PRIM_SYMBOLP, "symbol?", false),
    (PRIM_EQP, "eq?", false),
    (PRIM_EQUALP, "equal?", false),
    (PRIM_EQVP, "eqv?", false),
    (PRIM_NUMBERP, "number?", false),
    (PRIM_COMPLEXP, "complex?", false),
    (PRIM_REALP, "real?", false),
    (PRIM_RATIONALP, "rational?", false),
    (PRIM_INTEGERP, "integer?", false),
    (PRIM_EXACT_INTEGERP, "exact-integer?", false),
    (
        PRIM_EXACT_NONNEGATIVE_INTEGERP,
        "exact-nonnegative-integer?",
        false
    ),
    (
        PRIM_EXACT_POSITIVE_INTEGERP,
        "exact-positive-integer?",
        false
    ),
    (PRIM_INEXACT_REALP, "inexact-real?", false),
    (PRIM_FIXNUMP, "fixnum?", false),
    (PRIM_FLONUMP, "flonum?", false),
    (PRIM_ZERO, "zero?", true),
    (PRIM_LT, "<", true),
    (PRIM_LE, "<=", true),
    (PRIM_GT, ">", true),
    (PRIM_GE, ">=", true),
    (PRIM_EQ, "=", true),
    (PRIM_PLUS, "+", true),
    (PRIM_MINUS, "-", true),
    (PRIM_MULTIPLY, "*", true),
    (PRIM_DIVIDE, "/", true),
    (PRIM_MODULO, "modulo", true),
    (PRIM_DIVIDE_INTEGER, "integer-quotient", true),
    (PRIM_REMAINDER, "remainder", true),
    (PRIM_FLONUM_ADD, "flonum+", true),
    (PRIM_FLONUM_SUB, "flonum-", true),
    (PRIM_FLONUM_MUL, "flonum*", true),
    (PRIM_FLONUM_DIV, "flonum/", true),
    (PRIM_FLONUM_EQ, "flonum=", true),
    (PRIM_FLONUM_LT, "flonum<", true),
    (PRIM_FLONUM_LE, "flonum<=", true),
    (PRIM_FLONUM_GT, "flonum>", true),
    (PRIM_FLONUM_GE, "flonum>=", true),
    /* low-level primitives */
    (PRIM_ALLOC, "alloc", false),
    (PRIM_TYPECODE8, ".typecode8", false),
    (PRIM_TYPECODE16, ".typecode16", false),
    (PRIM_IS_IMMEDIATE, ".is-immediate", false),
    (PRIM_IS_CELL, ".is-cell", false),
    (PRIM_ICONST8, ".iconst", false),
    (PRIM_ICONST16, ".iconst16", false),
    (PRIM_ICONST32, ".iconst32", false),
    (PRIM_ICONST64, ".iconst64", false),
    (PRIM_REF8, ".ref8", false),
    (PRIM_REF16, ".ref16", false),
    (PRIM_REF32, ".ref32", false),
    (PRIM_REF64, ".ref64", false),
    (PRIM_REFPTR, ".refptr", false),
    (PRIM_SET8, ".set8", false),
    (PRIM_SET16, ".set16", false),
    (PRIM_SET32, ".set32", false),
    (PRIM_SET64, ".set64", false),
    (PRIM_SET_REF64, ".set-ref64", false),
    (PRIM_U8_ADD, "u8+", true),
    (PRIM_U8_SUB, "u8-", true),
    (PRIM_U8_MUL, "u8*", true),
    (PRIM_U8_DIV, "u8/", true),
    (PRIM_U8_SHL, "u8<<", true),
    (PRIM_U8_SHR, "u8>>", true),
    (PRIM_U8_AND, "u8and", true),
    (PRIM_U8_OR, "u8or", true),
    (PRIM_U8_XOR, "u8xor", true),
    (PRIM_U8_NOT, "u8not", true),
    (PRIM_U8_EQ, "u8=", true),
    (PRIM_U8_LT, "u8<", true),
    (PRIM_U8_LE, "u8<=", true),
    (PRIM_U8_GT, "u8>", true),
    (PRIM_U8_GE, "u8>=", true),
    (PRIM_U8_TO_U16, "u8->u16", true),
    (PRIM_U8_TO_U32, "u8->u32", true),
    (PRIM_U8_TO_U64, "u8->u64", true),
    (PRIM_U16_ADD, "u16+", true),
    (PRIM_U16_SUB, "u16-", true),
    (PRIM_U16_MUL, "u16*", true),
    (PRIM_U16_DIV, "u16/", true),
    (PRIM_U16_SHL, "u16<<", true),
    (PRIM_U16_SHR, "u16>>", true),
    (PRIM_U16_AND, "u16and", true),
    (PRIM_U16_OR, "u16or", true),
    (PRIM_U16_XOR, "u16xor", true),
    (PRIM_U16_NOT, "u16not", true),
    (PRIM_U16_EQ, "u16=", true),
    (PRIM_U16_LT, "u16<", true),
    (PRIM_U16_LE, "u16<=", true),
    (PRIM_U16_GT, "u16>", true),
    (PRIM_U16_GE, "u16>=", true),
    (PRIM_U16_TO_U8, "u16->u8", true),
    (PRIM_U16_TO_U32, "u16->u32", true),
    (PRIM_U16_TO_U64, "u16->u64", true),
    (PRIM_U32_ADD, "u32+", true),
    (PRIM_U32_SUB, "u32-", true),
    (PRIM_U32_MUL, "u32*", true),
    (PRIM_U32_DIV, "u32/", true),
    (PRIM_U32_SHL, "u32<<", true),
    (PRIM_U32_SHR, "u32>>", true),
    (PRIM_U32_AND, "u32and", true),
    (PRIM_U32_OR, "u32or", true),
    (PRIM_U32_XOR, "u32xor", true),
    (PRIM_U32_NOT, "u32not", true),
    (PRIM_U32_EQ, "u32=", true),
    (PRIM_U32_LT, "u32<", true),
    (PRIM_U32_LE, "u32<=", true),
    (PRIM_U32_GT, "u32>", true),
    (PRIM_U32_GE, "u32>=", true),
    (PRIM_U32_TO_U64, "u32->u64", true),
    (PRIM_U64_ADD, "u64+", true),
    (PRIM_U64_SUB, "u64-", true),
    (PRIM_U64_MUL, "u64*", true),
    (PRIM_U64_DIV, "u64/", true),
    (PRIM_U64_SHL, "u64<<", true),
    (PRIM_U64_SHR, "u64>>", true),
    (PRIM_U64_AND, "u64and", true),
    (PRIM_U64_OR, "u64or", true),
    (PRIM_U64_XOR, "u64xor", true),
    (PRIM_U64_NOT, "u64not", true),
    (PRIM_U64_EQ, "u64=", true),
    (PRIM_U64_LT, "u64<", true),
    (PRIM_U64_LE, "u64<=", true),
    (PRIM_U64_GT, "u64>", true),
    (PRIM_U64_GE, "u64>=", true),
    (PRIM_U64_TO_U8, "u64->u8", true),
    (PRIM_U64_TO_U16, "u64->u16", true),
    (PRIM_U64_TO_U32, "u64->u32", true),
    (PRIM_U32_TO_FIX, "u32->value", true),
    (PRIM_U64_TO_VALUE, "u64->value", true),
    (PRIM_USIZE_TO_VALUE, "usize->value", true),
    (PRIM_CALLCC, "call/cc", true),
    (PRIM_ERROR, "error", true),
    (PRIM_THROW, "throw", true)
);

/// Given a term, resolve all calls to primitives in it.
pub fn resolve_primitives<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    match &term.kind {
        TermKind::Call(proc, args) => {
            let args = args
                .iter()
                .map(|arg| resolve_primitives(ctx, arg.clone()))
                .collect::<Vec<_>>();

            if let TermKind::GRef(var) = proc.kind
                && primitives(ctx).set.contains_key(&var)
            {
                prim_call_term(ctx, var, &args, term.source)
            } else {
                let proc = resolve_primitives(ctx, proc.clone());

                call_term(ctx, proc, &args, term.source)
            }
        }

        TermKind::Values(values) => {
            let values = values
                .iter()
                .map(|v| resolve_primitives(ctx, v.clone()))
                .collect::<Vec<_>>();

            Gc::new(
                &ctx,
                Term {
                    source: term.source,
                    kind: TermKind::Values(Array::from_array(&ctx, values)),
                },
            )
        }

        TermKind::Receive(formals, opt_formal, producer, consumer) => {
            let producer = resolve_primitives(ctx, *producer);
            let consumer = resolve_primitives(ctx, *consumer);

            Gc::new(
                &ctx,
                Term {
                    source: term.source,
                    kind: TermKind::Receive(
                        formals.clone(),
                        opt_formal.clone(),
                        producer,
                        consumer,
                    ),
                },
            )
        }

        TermKind::Const(_) | TermKind::LRef(_) | TermKind::GRef(_) => term,

        TermKind::Define(var, val) => {
            let val = resolve_primitives(ctx, *val);
            Gc::new(
                &ctx,
                Term {
                    source: term.source,
                    kind: TermKind::Define(*var, val),
                },
            )
        }

        TermKind::Fix(fix) => {
            let rhs = fix
                .rhs
                .iter()
                .map(|proc| {
                    let body = resolve_primitives(ctx, proc.body);
                    Gc::new(
                        &ctx,
                        Proc {
                            name: proc.name,
                            body,

                            args: proc.args,
                            variadic: proc.variadic,
                            source: proc.source,
                        },
                    )
                })
                .collect::<Vec<_>>();
            let body = resolve_primitives(ctx, fix.body);

            Gc::new(
                &ctx,
                Term {
                    source: term.source,
                    kind: TermKind::Fix(Fix {
                        lhs: fix.lhs,
                        rhs: Array::from_array(&ctx, rhs),
                        body,
                    }),
                },
            )
        }

        TermKind::Let(let_term) => {
            let rhs = let_term
                .rhs
                .iter()
                .map(|val| {
                    let val = resolve_primitives(ctx, *val);
                    val
                })
                .collect::<Vec<_>>();

            let body = resolve_primitives(ctx, let_term.body.clone());

            Gc::new(
                &ctx,
                Term {
                    source: term.source,
                    kind: TermKind::Let(Let {
                        style: let_term.style,
                        lhs: let_term.lhs.clone(),
                        rhs: Array::from_array(&ctx, rhs),
                        body,
                    }),
                },
            )
        }

        TermKind::GSet(var, val) => {
            let val = resolve_primitives(ctx, *val);
            Gc::new(
                &ctx,
                Term {
                    source: term.source,
                    kind: TermKind::GSet(*var, val),
                },
            )
        }

        TermKind::If(test, cons, alt) => {
            let test = resolve_primitives(ctx, *test);
            let cons = resolve_primitives(ctx, *cons);
            let alt = resolve_primitives(ctx, *alt);

            Gc::new(
                &ctx,
                Term {
                    source: term.source,
                    kind: TermKind::If(test, cons, alt),
                },
            )
        }

        TermKind::PrimCall(proc, args) => {
            let args = args
                .iter()
                .map(|arg| resolve_primitives(ctx, arg.clone()))
                .collect::<Vec<_>>();

            prim_call_term(ctx, *proc, args, term.source)
        }

        TermKind::Seq(seq) => {
            let seq = seq
                .iter()
                .map(|term| resolve_primitives(ctx, term.clone()))
                .collect::<Vec<_>>();

            Gc::new(
                &ctx,
                Term {
                    source: term.source,
                    kind: TermKind::Seq(Array::from_array(&ctx, seq)),
                },
            )
        }

        TermKind::LSet(var, val) => {
            let val = resolve_primitives(ctx, *val);
            Gc::new(
                &ctx,
                Term {
                    source: term.source,
                    kind: TermKind::LSet(*var, val),
                },
            )
        }

        TermKind::Proc(proc) => {
            let body = resolve_primitives(ctx, proc.body);

            Gc::new(
                &ctx,
                Term {
                    source: term.source,
                    kind: TermKind::Proc(Gc::new(
                        &ctx,
                        Proc {
                            name: proc.name,
                            body,
                            args: proc.args,
                            variadic: proc.variadic,
                            source: proc.source,
                        },
                    )),
                },
            )
        }
    }
}
