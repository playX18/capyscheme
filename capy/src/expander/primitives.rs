use crate::cps::Set;
use crate::expander::core::{Fix, Let, Proc, Term, TermKind, TermRef, call_term, prim_call_term};
use crate::runtime::Context;
use crate::runtime::modules::{Module, resolve_module};
use crate::{runtime::value::Value, static_symbols};
use rsgc::cell::Lock;
use rsgc::traits::IterGc;
use rsgc::{Gc, Trace};
use rsgc::{Rootable, global::Global};
use std::collections::{HashMap, HashSet};
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
    pub interesting_variables: HashSet<Value<'gc>>,
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

                let module = $crate::runtime::modules::root_module(ctx);

                let mut vars = HashSet::<$crate::runtime::value::Value>::new();
                $(
                    let var = module.ensure_local_variable(ctx, $crate::runtime::value::Symbol::from_str(ctx, $l).into());
                    vars.insert(var.into());
                )*

                Global::new(Primitives {
                    interesting_variables: vars,
                    set
                })
            }).fetch(&ctx)
        }
    };
}

make_primitives!(
    (PRIM_BREAKPOINT, ".breakpoint", true),
    (PRIM_DEFINE, "define", true),
    (PRIM_BOXREF, "box-ref", true),
    (PRIM_BOXSET, "set-box!", true),
    (PRIM_BOX, "box", false),
    (PRIM_BOXP, "box?", false),
    (PRIM_SET_CAR, "set-car!", true),
    (PRIM_SET_CDR, "set-cdr!", true),
    //(PRIM_CAR, "car", true),
    //(PRIM_CDR, "cdr", true),
    (PRIM_CONS, "cons", true),
    (PRIM_NULLP, "null?", false),
    //(PRIM_PAIRP, "pair?", false),
    (PRIM_LIST, "list", false),
    (PRIM_VECTOR, "vector", false),
    //    (PRIM_VECTORP, "vector?", false),
    //(PRIM_VECTOR_REF, "vector-ref", true),
    // (PRIM_VECTOR_SET, "vector-set!", true),
    (PRIM_STRINGP, "string?", false),
    //(PRIM_STRING_LENGTH, "string-length", true),
    //(PRIM_STRING_REF, "string-ref", true),
    //(PRIM_STRING_SET, "string-set!", true),
    //(PRIM_NOT, "not", false),
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
    (PRIM_TYPECODE8, ".typecode8", false),
    (PRIM_TYPECODE16, ".typecode16", false),
    (PRIM_IS_IMMEDIATE, ".is-immediate", false),
    (PRIM_IS_HEAP_OBJECT, ".is-heap-object", false),
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
    (PRIM_LOOKUP, "lookup", true),
    (PRIM_LOOKUP_BOUND, "lookup-bound", true),
    (PRIM_CURRENT_MODULE, "current-module", true),
    (PRIM_CACHE_REF, "cache-ref", true),
    (PRIM_CACHE_SET, "cache-set!", true),
    (PRIM_TUPLE, "tuple", false),
    (PRIM_MAKE_TUPLE, "make-tuple", false),
    (PRIM_TUPLE_SIZE, "tuple-size", false),
    (PRIM_TUPLE_REF, "tuple-ref", false),
    (PRIM_TUPLE_SET, "tuple-set!", false),
    (PRIM_IS_TUPLE, "tuple?", false)
);

/*
/// Given a term, resolve all calls to primitives in it.
pub fn resolve_primitives<'gc>(ctx: Context<'gc>, term: TermRef<'gc>) -> TermRef<'gc> {
    match &term.kind {
        TermKind::Call(proc, args) => {
            let args = args
                .iter()
                .map(|arg| resolve_primitives(ctx, arg.clone()))
                .collect::<Vec<_>>();

            if let TermKind::ToplevelRef(var) = proc.kind
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
                    kind: TermKind::Values(Array::from_slice(&ctx, values)),
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
                        rhs: Array::from_slice(&ctx, rhs),
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
                        rhs: Array::from_slice(&ctx, rhs),
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
                    kind: TermKind::Seq(Array::from_slice(&ctx, seq)),
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
*/

pub fn resolve_primitives<'gc>(
    ctx: Context<'gc>,
    t: TermRef<'gc>,
    m: Gc<'gc, Module<'gc>>,
) -> TermRef<'gc> {
    let _ = primitives(ctx);
    let mut resolver = Resolver::new(ctx, t, m);
    rec(&mut resolver, ctx, t)
}

fn rec<'gc>(r: &mut Resolver<'gc>, ctx: Context<'gc>, t: TermRef<'gc>) -> TermRef<'gc> {
    match t.kind {
        TermKind::ToplevelRef(_, name) => {
            if !r.local_definitions.contains(&name)
                && r.m.variable(ctx, name).map_or(false, |var| {
                    primitives(ctx).interesting_variables.contains(&var.into())
                })
            {
                Gc::new(
                    &ctx,
                    Term {
                        source: Lock::new(t.source()),
                        kind: TermKind::PrimRef(name),
                    },
                )
            } else {
                t
            }
        }

        TermKind::ModuleRef(module, name, public) => {
            let Some(module) = resolve_module(ctx, module, false, true) else {
                return t;
            };

            let i = if public {
                module.public_interface.get().unwrap_or(module)
            } else {
                module
            };

            let Some(v) = i.variable(ctx, name) else {
                return t;
            };

            if primitives(ctx).interesting_variables.contains(&v.into()) {
                Gc::new(
                    &ctx,
                    Term {
                        source: Lock::new(t.source()),
                        kind: TermKind::PrimRef(name),
                    },
                )
            } else {
                t
            }
        }

        TermKind::Call(proc, args) => {
            let proc = rec(r, ctx, proc);
            let args = args.iter().map(|arg| rec(r, ctx, *arg)).collect::<Vec<_>>();
            if let TermKind::PrimRef(name) = proc.kind {
                prim_call_term(ctx, name, &args, t.source())
            } else {
                call_term(ctx, proc, &args, t.source())
            }
        }

        TermKind::Const(_) => t,
        TermKind::Define(module, name, val) => {
            let val = rec(r, ctx, val);

            return Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::Define(module, name, val),
                },
            );
        }

        TermKind::Fix(fix) => {
            let rhs = fix
                .rhs
                .iter()
                .map(|p| {
                    let body = rec(r, ctx, p.body);
                    Gc::new(
                        &ctx,
                        Proc {
                            name: p.name,
                            body,
                            args: p.args,
                            variadic: p.variadic,
                            source: p.source,
                        },
                    )
                })
                .collect_gc(&ctx);
            let body = rec(r, ctx, fix.body);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::Fix(Fix {
                        body,
                        rhs,
                        lhs: fix.lhs,
                    }),
                },
            )
        }

        TermKind::If(test, cons, alt) => {
            let test = rec(r, ctx, test);
            let cons = rec(r, ctx, cons);
            let alt = rec(r, ctx, alt);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::If(test, cons, alt),
                },
            )
        }

        TermKind::LRef(_) => t,
        TermKind::LSet(var, val) => {
            let val = rec(r, ctx, val);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::LSet(var, val),
                },
            )
        }

        TermKind::Let(l) => {
            let rhs = l.rhs.iter().map(|v| rec(r, ctx, *v)).collect_gc(&ctx);
            let lhs = l.lhs;
            let style = l.style;
            let body = rec(r, ctx, l.body);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::Let(Let {
                        lhs,
                        rhs,
                        body,
                        style,
                    }),
                },
            )
        }

        TermKind::ModuleSet(module, name, public, exp) => {
            let exp = rec(r, ctx, exp);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::ModuleSet(module, name, public, exp),
                },
            )
        }

        TermKind::PrimCall(name, args) => {
            let args = args.iter().map(|arg| rec(r, ctx, *arg)).collect_gc(&ctx);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::PrimCall(name, args),
                },
            )
        }

        TermKind::PrimRef(_) => t,
        TermKind::Proc(p) => {
            let body = rec(r, ctx, p.body);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::Proc(Gc::new(
                        &ctx,
                        Proc {
                            name: p.name,
                            body,
                            args: p.args,
                            variadic: p.variadic,
                            source: p.source,
                        },
                    )),
                },
            )
        }

        TermKind::Receive(args, variadic, receiver, producer) => {
            let receiver = rec(r, ctx, receiver);
            let producer = rec(r, ctx, producer);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::Receive(args, variadic, receiver, producer),
                },
            )
        }

        TermKind::Seq(seq) => {
            let seq = seq.iter().map(|t| rec(r, ctx, *t)).collect_gc(&ctx);

            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::Seq(seq),
                },
            )
        }

        TermKind::ToplevelSet(module, var, val) => {
            let val = rec(r, ctx, val);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::ToplevelSet(module, var, val),
                },
            )
        }

        TermKind::Values(vals) => {
            let vals = vals.iter().map(|v| rec(r, ctx, *v)).collect_gc(&ctx);
            Gc::new(
                &ctx,
                Term {
                    source: Lock::new(t.source()),
                    kind: TermKind::Values(vals),
                },
            )
        }
    }
}

struct Resolver<'gc> {
    local_definitions: Set<Value<'gc>>,
    m: Gc<'gc, Module<'gc>>,
}

impl<'gc> Resolver<'gc> {
    fn new(_ctx: Context<'gc>, t: TermRef<'gc>, m: Gc<'gc, Module<'gc>>) -> Self {
        let mut local_definitions = Set::default();

        collect_local_definitions(t, &mut local_definitions);

        Resolver {
            local_definitions,
            m,
        }
    }
}

fn collect_local_definitions<'gc>(x: TermRef<'gc>, set: &mut Set<Value<'gc>>) {
    match x.kind {
        TermKind::Define(_, var, _) => {
            set.insert(var);
        }

        TermKind::Seq(seq) => {
            for exp in seq.iter() {
                collect_local_definitions(*exp, set);
            }
        }

        _ => (),
    }
}
