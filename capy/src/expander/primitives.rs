use crate::cps::Set;
use crate::expander::core::{
    LetStyle, Term, TermKind, TermRef, call_term, constant, fresh_lvar, if_term, let_term, lref,
    prim_call_term, seq_from_slice,
};
use crate::list;
use crate::rsgc::Gc;
use crate::rsgc::alloc::Array;
use crate::rsgc::cell::Lock;
use crate::runtime::Context;
use crate::runtime::modules::{Module, Variable, resolve_module};
use crate::runtime::prelude::*;
use crate::runtime::value::Symbol;
use crate::{runtime::value::Value, static_symbols};
use std::collections::HashMap;

macro_rules! interesting_prim_names {
    ($($sname: ident =  $name: literal)*) => {
        paste::paste! {
            static_symbols! {
                $(
                    [<SYM_ $sname: upper>] = $name
                )*
            }

            pub fn for_each_prim_name<'gc, F: FnMut(&'static str, Value<'gc>)>(ctx: Context<'gc>, mut f: F) {
                $(
                    let name = paste::paste! {
                        [<sym_ $sname: lower>](ctx)
                    };
                    f($name, name.into());
                )*
            }
        }
    };
}

interesting_prim_names!(
    apply = "apply"
    call_with_values = "call-with-values"
    ccm = "current-continuation-marks"
    set_ccm = "$set-attachments!"
    values = "values"
    eqp = "eq?"
    eqvp = "eqv?"
    equalp = "equal?"
    memq = "memq"
    memv = "memv"
    // math
    number_eq = "="
    number_lt = "<"
    number_le = "<="
    number_gt = ">"
    number_ge = ">="
    plus = "+"
    minus = "-"
    multiply = "*"
    div = "/"
    quotient = "quotient"
    remainder = "remainder"
    modulo = "modulo"
    exact_to_inexact = "exact->inexact"
    inexact_to_exact = "inexact->exact"
    expt = "expt"
    ash = "ash"
    //logand = "logand"
    //logior = "logior"
    //logxor = "logxor"
    //lognot = "lognot"
    logtest = "logtest"
    logibtp = "logbit?"
    sqrt = "sqrt"
    abs = "abs"
    floor = "floor"
    ceiling = "ceiling"
    sin = "sin"
    cos = "cos"
    tan = "tan"
    asin = "asin"
    acos = "acos"
    atan = "atan"
    // booleans
    not = "not"

    // predicates
    pairp = "pair?"
    nullp = "null?"
    listp = "list?"
    symbolp = "symbol?"
    vectorp = "vector?"
    stringp = "string?"
    numberp = "number?"
    charp = "char?"
    booleanp = "boolean?"
    nilp = "nil?"
    eof_objectp = "eof-object?"
    tuplep = "tuple?"
    bytevectorp = "bytevector?"

    symbol2string = "symbol->string"
    string2symbol = "string->symbol"

    procedurep = "procedure?"

    complexp = "complex?"
    realp = "real?"
    rationalp = "rational?"
    infp = "inf?"
    nanp = "nan?"
    integerp = "integer?"
    exactp = "exact?"
    inexactp = "inexact?"
    evenp = "even?"
    oddp = "odd?"
    zerop = "zero?"
    positivep = "positive?"
    negativep = "negative?"
    exact_integerp = "exact-integer?"

    char_lt = "char<?"
    char_le = "char<=?"
    char_gt = "char>?"
    char_ge = "char>=?"
    char_equal = "char=?"

    integer2char = "integer->char"
    char2integer = "char->integer"


    acons = "acons"
    cons = "cons"
    cons_star = "cons*"
    append = "append"
    make_syntax = "make-syntax"

    list = "list"
    vector = "vector"
    tuple = "tuple"


    car = "car"
    cdr = "cdr"
    set_car = "set-car!"
    set_cdr = "set-cdr!"

    caar = "caar"
    cadr = "cadr"
    cdar = "cdar"
    cddr = "cddr"

    caaar = "caaar"
    caadr = "caadr"
    cadar = "cadar"
    caddr = "caddr"
    cdaar = "cdaar"
    cdadr = "cdadr"
    cddar = "cddar"
    cdddr = "cdddr"

    caaaar = "caaaar"
    caaadr = "caaadr"
    caadar = "caadar"
    caaddr = "caaddr"
    cadaar = "cadaar"
    cadadr = "cadadr"
    caddar = "caddar"
    cadddr = "cadddr"
    cdaaar = "cdaaar"
    cdaadr = "cdaadr"
    cdadar = "cdadar"
    cdaddr = "cdaddr"
    cddaar = "cddaar"
    cddadr = "cddadr"
    cdddar = "cdddar"
    cddddr = "cddddr"

    length = "length"

    make_vector = "make-vector"
    vector_length = "vector-length"
    vector_ref = "vector-ref"
    vector_set = "vector-set!"

    make_tuple = "make-tuple"
    tuple_size = "tuple-size"
    tuple_ref = "tuple-ref"
    tuple_set = "tuple-set!"

    variablep = "variable?"
    variable_ref = "variable-ref"
    variable_set = "variable-set!"
    make_variable = "make-variable"
    variable_bound = "variable-bound?"


    current_module = "current-module"
    define = "define!"

    string_length = "string-length"
    string_ref = "string-ref"
    string_set = "string-set!"

    bytevector_length = "bytevector-length"
    bytevector_u8_ref = "bytevector-u8-ref"
    bytevector_u8_set = "bytevector-u8-set!"
    bytevector_s8_ref = "bytevector-s8-ref"
    bytevector_s8_set = "bytevector-s8-set!"

    bytevector_u16_ref = "bytevector-u16-ref"
    bytevector_u16_set = "bytevector-u16-set!"
    bytevector_s16_ref = "bytevector-s16-ref"
    bytevector_s16_set = "bytevector_s16-set!"
    bytevector_u16_native_ref = "bytevector-u16-native-ref"
    bytevector_u16_native_set = "bytevector-u16-native-set!"
    bytevector_s16_native_ref = "bytevector-s16-native-ref"
    bytevector_s16_native_set = "bytevector-s16-native-set!"

    bytevector_u32_ref = "bytevector-u32-ref"
    bytevector_u32_set = "bytevector-u32-set!"
    bytevector_s32_ref = "bytevector-s32-ref"
    bytevector_s32_set = "bytevector-s32-set!"
    bytevector_u32_native_ref = "bytevector-u32-native-ref"
    bytevector_u32_native_set = "bytevector-u32-native-set!"
    bytevector_s32_native_ref = "bytevector-s32-native-ref"
    bytevector_s32_native_set = "bytevector-s32-native-set!"

    bytevector_u64_ref = "bytevector-u64-ref"
    bytevector_u64_set = "bytevector-u64-set!"
    bytevector_s64_ref = "bytevector-s64-ref"
    bytevector_s64_set = "bytevector-s64-set!"
    bytevector_u64_native_ref = "bytevector-u64-native-ref"
    bytevector_u64_native_set = "bytevector-u64-native-set!"
    bytevector_s64_native_ref = "bytevector-s64-native-ref"
    bytevector_s64_native_set = "bytevector-s64-native-set!"

    winders = "$winders"

    callcc = "call/cc"
    call_with_current_continuation = "call-with-current-continuation"

    unspecified = "unspecified"
    unspecifiedp = "unspecified?"
);

pub fn resolve_primitives<'gc>(
    ctx: Context<'gc>,
    x: TermRef<'gc>,
    m: Gc<'gc, Module<'gc>>,
) -> TermRef<'gc> {
    let mut local_definitions = Set::default();
    if !Gc::ptr_eq(m, ctx.globals().root_module()) {
        fn collect_local_definitions<'gc>(
            x: TermRef<'gc>,
            local_definitions: &mut Set<Value<'gc>>,
        ) {
            match &x.kind {
                TermKind::Define(_, name, _) => {
                    local_definitions.insert(*name);
                }

                TermKind::Seq(head, tail) => {
                    /*for exp in seq.iter() {
                        collect_local_definitions(*exp, local_definitions);
                    }*/
                    collect_local_definitions(*head, local_definitions);
                    collect_local_definitions(*tail, local_definitions);
                }
                _ => (),
            }
        }
        collect_local_definitions(x, &mut local_definitions);
    }

    x.post_order(ctx, |ctx, x| match &x.kind {
        TermKind::ToplevelRef(_, name) => {
            if let Some(var) = m.variable(ctx, *name)
                && let Some(prim) = ctx.globals().interesting_primitive_vars().get(ctx, *name)
                && Gc::ptr_eq(var, prim.downcast::<Variable>())
                && !local_definitions.contains(&name)
            {
                Gc::new(
                    *ctx,
                    Term {
                        source: Lock::new(x.source()),
                        kind: TermKind::PrimRef(*name),
                    },
                )
            } else {
                x
            }
        }

        TermKind::ModuleRef(module, name, public) => {
            if let Some(module) = resolve_module(ctx, *module, false, false) {
                let iface = if *public {
                    module.public_interface.get().unwrap_or(module)
                } else {
                    module
                };

                if let Some(v) = iface.variable(ctx, *name)
                    && let Some(name) = ctx.globals().interesting_primitive_vars().get(ctx, v)
                {
                    return Gc::new(
                        *ctx,
                        Term {
                            source: Lock::new(x.source()),
                            kind: TermKind::PrimRef(name),
                        },
                    );
                }
            }

            x
        }

        TermKind::Call(proc, args) => {
            if let TermKind::PrimRef(name) = proc.kind {
                prim_call_term(ctx, name, args.as_slice(), x.source())
            } else {
                x
            }
        }

        _ => x,
    })
}

macro_rules! primitive_expanders {
    ($($name: literal $fname: ident <$gc: lifetime>($ctx : ident, $args: ident, $src: ident) $b: block)*) => {
        $(
            #[allow(unused)]
            fn $fname<$gc>($ctx: Context<$gc>, $args: &[TermRef<$gc>], $src: Value<$gc>) -> Option<TermRef<$gc>> $b
        )*

        type Expander = for<'gc> fn(Context<'gc>, &[TermRef<'gc>], Value<'gc>) -> Option<TermRef<'gc>>;


        struct ExpanderTable {
            table: HashMap<&'static str, Expander>,
        }

        impl ExpanderTable {
            fn new() -> Self {
                let mut table = HashMap::new();
                $(
                    table.insert($name, $fname as Expander);
                )*
                ExpanderTable { table }
            }

            fn get<'gc>(&self, _ctx: Context<'gc>, name: SymbolRef<'gc>) -> Option<Expander> {
                let key = name.as_str();
                self.table.get(&*key).copied()
            }
        }

        use std::sync::LazyLock;

        static PRIMITIVE_EXPANDERS: LazyLock<ExpanderTable> = LazyLock::new(|| {
            ExpanderTable::new()
        });
    };
}

primitive_expanders!(

    "current-continuation-marks" ex_current_continuation_marks<'gc>(ctx, args, src) {
        if !args.is_empty() {
            return None;
        }

        Some(prim_call_term(ctx, sym_ccm(ctx).into(), &[], src))
    }

    "$set-attachments!" ex_set_ccm<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_set_ccm(ctx).into(), args, src))
    }

    "apply" ex_apply<'gc>(ctx, args, src) {
        // TODO: expand apply into a call when possible?
        // Right now just reify into a normal call...
        let _ = args;
        let _ = src;
        let _ = ctx;
        return None;
    }

    // expands `(call-with-values producer consumer)`
    // into `(receive <consmer.args> <producer.body> <consumer.body>)`
    // assuming both values are procedures and producer takes no arguments
    "call-with-values" ex_call_with_values<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        let TermKind::Proc(producer) = args[0].kind else {
            return None;
        };

        let TermKind::Proc(consumer) = args[1].kind else {
            return None;
        };

        // producer can't accept any arguments
        if producer.variadic.is_some() || !producer.args.is_empty() {
            return None;
        }


        Some(Gc::new(
            *ctx,
            Term {
                source: src.into(),
                kind: TermKind::Receive(
                    consumer.args.clone(),
                    consumer.variadic,
                    producer.body,
                    consumer.body,
                ),
            },
        ))
    }

    "eq?" ex_eqp<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_eqp(ctx).into(), args, src))
    }

    "eqv?" ex_eqvp<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_eqvp(ctx).into(), args, src))
    }

    "equal?" ex_equalp<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_equalp(ctx).into(), args, src))
    }

    "memv" ex_memv<'gc>(ctx, args, src) {
        /*
            (memv key '(v1 v2 v3)) =>
            (or (eqv? key v1) (eqv? (key v2) (eqv? key v3)))
        */
        if args.len() != 2 {
            return None;
        }

        let key = args[0];
        let list = args[1];

        let TermKind::Const(ls) = list.kind else {
            return None;
        };

        if !ls.is_list() {
            return None;
        }

        if ls.is_null() {
            let x = constant(ctx, Value::new(false));
            // effects are preserved by returning (begin key false)

            return Some(seq_from_slice(ctx, [key, x]));
        }

        let len = ls.list_length();
        if len == 1 {
            let v1 = ls.list_ref(0).unwrap();
            let check = prim_call_term(ctx, sym_eqp(ctx).into(), &[key, constant(ctx, v1)], src);
            return Some(if_term(ctx, check, constant(ctx, ls), constant(ctx, Value::new(false))));
        } else if len > 5 {
            // too much elements to check: reify into a normal call
            return None;
        }

        let mut it = ls;
        let mut result = constant(ctx, Value::new(false));

        while it.is_pair() {
            let val = it.car();
            let rest_val = it;
            it = it.cdr();

            let check = prim_call_term(ctx, sym_eqvp(ctx).into(), &[key, constant(ctx, val)], src);
            result = if_term(ctx, check, constant(ctx, rest_val), result);
        }

        Some(result)
    }

    "memq" ex_memq<'gc>(ctx, args, src) {
        /*
            (memq key '(v1 v2 v3)) =>
            (or (and (eq? key 'v1) '(v1 v2 v3)
                (and (eq? key 'v2) '(v2 v3)
                    (and (eq? key 'v3) '(v3))))
        */
        if args.len() != 2 {
            return None;
        }

        let key = args[0];
        let list = args[1];

        let TermKind::Const(ls) = list.kind else {
            return None;
        };

        if !ls.is_list() {
            return None;
        }

        if ls.is_null() {
            let x = constant(ctx, Value::new(false));
            // effects are preserved by returning (begin key false)

            return Some(seq_from_slice(ctx, [key, x]));
        }

        let len = ls.list_length();
        if len == 1 {
            let v1 = ls.list_ref(0).unwrap();
            let check = prim_call_term(ctx, sym_eqp(ctx).into(), &[key, constant(ctx, v1)], src);
            return Some(if_term(ctx, check, constant(ctx, ls), constant(ctx, Value::new(false))));
        } else if len > 5 {
            // too much elements to check: reify into a normal call
            return None;
        }

        let mut it = ls;
        let mut result = constant(ctx, Value::new(false));

        while it.is_pair() {
            let val = it.car();
            let rest_val = it;
            it = it.cdr();

            let check = prim_call_term(ctx, sym_eqp(ctx).into(), &[key, constant(ctx, val)], src);
            result = if_term(ctx, check, constant(ctx, rest_val), result);
        }

        Some(result)
    }


    "=" ex_num_eq<'gc>(ctx, args, src) {
        multi_compare(ctx, src, "=", args, false, false)
    }

    "<" ex_num_lt<'gc>(ctx, args, src) {
        multi_compare(ctx, src, "<", args, false, false)
    }

    ">" ex_num_gt<'gc>(ctx, args, src) {
        multi_compare(ctx, src, ">", args, false, false)
    }

    ">=" ex_num_ge<'gc>(ctx, args, src) {
        multi_compare(ctx, src, ">=", args, false, false)
    }

    "<=" ex_num_le<'gc>(ctx, args, src) {
        multi_compare(ctx, src, "<=", args, false, false)
    }


    "+" ex_plus<'gc>(ctx, args, src) {
        transitive(ctx, src, "+", args, Some(Value::new(0i32)), true, None)
    }

    "*" ex_multiply<'gc>(ctx, args, src) {
        transitive(ctx, src, "*", args, Some(Value::new(1i32)), true, None)
    }

    "-" ex_minus<'gc>(ctx, args, src) {
        transitive(ctx, src, "-", args, None, true, Some(|ctx, src, arg| {
            Term::prims(ctx, "-", [constant(ctx, 0i32.into()), arg], src)
        }))
    }

    "/" ex_div<'gc>(ctx, args, src) {
        transitive(ctx, src, "/", args, None, true, Some(|ctx, src, arg|
            Term::prims(ctx, "/", [constant(ctx, 1i32.into()), arg], src)
        ))
    }

    "quotient" ex_quotient<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_quotient(ctx).into(), args, src))
    }

    "remainder" ex_remainder<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_remainder(ctx).into(), args, src))
    }

    "modulo" ex_modulo<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_modulo(ctx).into(), args, src))
    }

    "exact->inexact" ex_exact_to_inexact<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_exact_to_inexact(ctx).into(), args, src))
    }

    "inexact->exact" ex_inexact_to_exact<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_inexact_to_exact(ctx).into(), args, src))
    }

    "expt" ex_expt<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_expt(ctx).into(), args, src))
    }

    "ash" ex_ash<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_ash(ctx).into(), args, src))
    }

/*     "logand" ex_logand<'gc>(ctx, args, src) {
        if args.is_empty() {
            return Some(constant(ctx, Value::new(-1i32)));
        } else if args.len() == 1 {
            return Some(args[0]);
        } else if args.len() == 2 {
            return Some(prim_call_term(ctx, sym_logand(ctx).into(), args, src));
        } else {
            let first = args[0];
            let rest = &args[1..];
            return Some(prim_call_term(ctx, sym_logand(ctx).into(), &[first, ex_logand(ctx, rest, src)?], src));
        }
    }

    "logior" ex_logior<'gc>(ctx, args, src) {
        if args.is_empty() {
            return Some(constant(ctx, Value::new(0i32)));
        } else if args.len() == 1 {
            return Some(args[0]);
        } else if args.len() == 2 {
            return Some(prim_call_term(ctx, sym_logior(ctx).into(), args, src));
        } else {
            let first = args[0];
            let rest = &args[1..];
            return Some(prim_call_term(ctx, sym_logior(ctx).into(), &[first, ex_logior(ctx, rest, src)?], src));
        }
    }

    "logxor" ex_logxor<'gc>(ctx, args, src) {
        if args.is_empty() {
            return Some(constant(ctx, Value::new(0i32)));
        } else if args.len() == 1 {
            return Some(args[0]);
        } else if args.len() == 2 {
            return Some(prim_call_term(ctx, sym_logxor(ctx).into(), args, src));
        } else {
            let first = args[0];
            let rest = &args[1..];
            return Some(prim_call_term(ctx, sym_logxor(ctx).into(), &[first, ex_logxor(ctx, rest, src)?], src));
        }
    }

    "lognot" ex_lognot<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_lognot(ctx).into(), args, src))
    }
*/
    "logtest" ex_logtest<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_logtest(ctx).into(), args, src))
    }

    "logbit?" ex_logibtp<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_logibtp(ctx).into(), args, src))
    }

    "sqrt" ex_sqrt<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_sqrt(ctx).into(), args, src))
    }

    "abs" ex_abs<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_abs(ctx).into(), args, src))
    }

    "floor" ex_floor<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_floor(ctx).into(), args, src))
    }

    "ceiling" ex_ceiling<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_ceiling(ctx).into(), args, src))
    }

    "sin" ex_sin<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_sin(ctx).into(), args, src))
    }

    "cos" ex_cos<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cos(ctx).into(), args, src))
    }

    "tan" ex_tan<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_tan(ctx).into(), args, src))
    }

    "asin" ex_asin<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_asin(ctx).into(), args, src))
    }

    "acos" ex_acos<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_acos(ctx).into(), args, src))
    }

    "atan" ex_atan<'gc>(ctx, args, src) {
        if args.len() != 1 && args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_atan(ctx).into(), args, src))
    }

    "not" ex_not<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_not(ctx).into(), args, src))
    }

    "pair?" ex_pairp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_pairp(ctx).into(), args, src))
    }

    "null?" ex_nullp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_nullp(ctx).into(), args, src))
    }

    "list?" ex_listp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_listp(ctx).into(), args, src))
    }

    "vector?" ex_vectorp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_vectorp(ctx).into(), args, src))
    }

    "string?" ex_stringp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_stringp(ctx).into(), args, src))
    }

    "number?" ex_numberp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_numberp(ctx).into(), args, src))
    }

    "char?" ex_charp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_charp(ctx).into(), args, src))
    }

    "boolean?" ex_booleanp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_booleanp(ctx).into(), args, src))
    }

    "eof-object?" ex_eof_objectp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_eof_objectp(ctx).into(), args, src))
    }

    "tuple?" ex_tuplep<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_tuplep(ctx).into(), args, src))
    }

    "bytevector?" ex_bytevectorp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_bytevectorp(ctx).into(), args, src))
    }

    "symbol->string" ex_symbol_to_string<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_symbol2string(ctx).into(), args, src))
    }

    "string->symbol" ex_string_to_symbol<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_string2symbol(ctx).into(), args, src))
    }

    "procedure?" ex_procedurep<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_procedurep(ctx).into(), args, src))
    }

    "complex?" ex_complexp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_complexp(ctx).into(), args, src))
    }

    "real?" ex_realp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_realp(ctx).into(), args, src))
    }

    "rational?" ex_rationalp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_rationalp(ctx).into(), args, src))
    }

    "inf?" ex_infp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_infp(ctx).into(), args, src))
    }

    "nan?" ex_nanp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_nanp(ctx).into(), args, src))
    }

    "integer?" ex_integerp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_integerp(ctx).into(), args, src))
    }

    "exact?" ex_exactp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_exactp(ctx).into(), args, src))
    }

    "inexact?" ex_inexactp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_inexactp(ctx).into(), args, src))
    }

    "even?" ex_evenp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_evenp(ctx).into(), args, src))
    }

    "odd?" ex_oddp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_oddp(ctx).into(), args, src))
    }

    "zero?" ex_zero<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        let zero = constant(ctx, Value::new(0i32));

        Some(prim_call_term(ctx, sym_number_eq(ctx).into(), &[args[0], zero], src))
    }

    "positive?" ex_positive<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_number_gt(ctx).into(), &[args[0], constant(ctx, Value::new(0i32))], src))
    }

    "negative?" ex_negative<'gc>(ctx, args, src) {

        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_number_lt(ctx).into(), &[args[0], constant(ctx, Value::new(0i32))], src))
    }


    "exact-integer?" ex_exact_integerp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_exact_integerp(ctx).into(), args, src))
    }

    "char=?" ex_char_eq<'gc>(ctx, args, src) {
        if args.len() < 2 {
            return None;
        }

        // replace with `(= (char->integer arg0) ...)`
        let mut int_args = Vec::with_capacity(args.len());
        for arg in args {
            int_args.push(prim_call_term(ctx, sym_char2integer(ctx).into(), &[
                *arg
            ], src));
        }

        Some(prim_call_term(ctx, sym_number_eq(ctx).into(), &int_args, src))
    }

    "char<?" ex_char_lt<'gc>(ctx, args, src) {
        if args.len() < 2 {
            return None;
        }

        // replace with `(< (char->integer arg0) ...)`
        let mut int_args = Vec::with_capacity(args.len());
        for arg in args {
            int_args.push(prim_call_term(ctx, sym_char2integer(ctx).into(), &[
                *arg
            ], src));
        }

        ex_num_lt(ctx, &int_args, src)
    }

    "char<=?" ex_char_le<'gc>(ctx, args, src) {
        if args.len() < 2 {
            return None;
        }

        // replace with `(<= (char->integer arg0) ...)`
        let mut int_args = Vec::with_capacity(args.len());
        for arg in args {
            int_args.push(prim_call_term(ctx, sym_char2integer(ctx).into(), &[
                *arg
            ], src));
        }

        ex_num_le(ctx, &int_args, src)
    }

    "char>?" ex_char_gt<'gc>(ctx, args, src) {
        if args.len() < 2 {
            return None;
        }

        // replace with `(> (char->integer arg0) ...)`
        let mut int_args = Vec::with_capacity(args.len());
        for arg in args {
            int_args.push(prim_call_term(ctx, sym_char2integer(ctx).into(), &[
                *arg
            ], src));
        }

        ex_num_gt(ctx, &int_args, src)
    }

    "char>=?" ex_char_ge<'gc>(ctx, args, src) {
        if args.len() < 2 {
            return None;
        }

        // replace with `(>= (char->integer arg0) ...)`
        let mut int_args = Vec::with_capacity(args.len());
        for arg in args {
            int_args.push(prim_call_term(ctx, sym_char2integer(ctx).into(), &[
                *arg
            ], src));
        }

        ex_num_ge(ctx, &int_args, src)
    }

    "char->integer" ex_char_to_integer<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_char2integer(ctx).into(), args, src))
    }

    "integer->char" ex_integer_to_char<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_integer2char(ctx).into(), args, src))
    }


    "cons" ex_cons<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cons(ctx).into(), args, src))
    }

    "cons*" ex_cons_star<'gc>(ctx, args, src) {
        if args.is_empty() {
            return None;
        } else if args.len() == 1 {
            Some(args[0])
        } else if args.len() == 2 {
            Some(prim_call_term(ctx, sym_cons(ctx).into(), args, src))
        } else {
            let first = args[0];
            let rest = &args[1..];
            Some(prim_call_term(ctx, sym_cons(ctx).into(), &[first, ex_cons_star(ctx, rest, src)?], src))
        }
    }

    "append" ex_append<'gc>(ctx, args, src) {
        if args.is_empty() {
            Some(constant(ctx, Value::null()))
        } else if args.len() == 1 {
            Some(args[0])
        } else if args.len() == 2 {
            Some(prim_call_term(ctx, sym_append(ctx).into(), args, src))
        } else {
            let x = args[0];

            let rest = &args[1..];
            // (append x (append ...rest))
            Some(prim_call_term(ctx, sym_append(ctx).into(), &[x, ex_append(ctx, &rest, src)?], src))
        }
    }

    "acons" ex_acons<'gc>(ctx, args, src) {
        if args.len() != 3 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cons(ctx).into(), &[prim_call_term(ctx, sym_cons(ctx).into(), &[args[0], args[1]], src), args[2]], src))
    }



    "set-car!" ex_set_car<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_set_car(ctx).into(), args, src))
    }

    "set-cdr!" ex_set_cdr<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }

        Some(prim_call_term(ctx, sym_set_cdr(ctx).into(), args, src))
    }

    "car" ex_car<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), args, src))
    }

    "cdr" ex_cdr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), args, src))
    }

    "caar" ex_caar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src))
    }

    "cadr" ex_cadr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src))
    }

    "cdar" ex_cdar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src))
    }

    "cddr" ex_cddr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src))
    }

    "caaar" ex_caaar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src))
    }

    "caadr" ex_caadr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src))
    }

    "cadar" ex_cadar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src))
    }

    "caddr" ex_caddr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src))
    }

    "cdaar" ex_cdaar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src))
    }

    "cdadr" ex_cdadr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src))
    }

    "cddar" ex_cddar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src))
    }

    "cdddr" ex_cdddr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src))
    }

    "caaaar" ex_caaaar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "caaadr" ex_caaadr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "caadar" ex_caadar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "caaddr" ex_caaddr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "cadaar" ex_cadaar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "cadadr" ex_cadadr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "caddar" ex_caddar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "cadddr" ex_cadddr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "cdaaar" ex_cdaaar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "cdaadr" ex_cdaadr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "cdadar" ex_cdadar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "cdaddr" ex_cdaddr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "cddaar" ex_cddaar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "cddadr" ex_cddadr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "cdddar" ex_cdddar<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_car(ctx).into(), &[args[0]], src)], src)], src)], src))
    }

    "cddddr" ex_cddddr<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[prim_call_term(ctx, sym_cdr(ctx).into(), &[args[0]], src)], src)], src)], src))
    }


    "length" ex_length<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }

        Some(prim_call_term(ctx, sym_length(ctx).into(), args, src))
    }


    "list" ex_list<'gc>(ctx, args, src) {
        // replace (list a b c) with (cons a (cons b (cons c '())))
        if args.is_empty() {
            return Some(constant(ctx, Value::null()));
        } else if args.len() == 1 {
            return Some(prim_call_term(ctx, sym_cons(ctx).into(), &[args[0], constant(ctx, Value::null())], src));
        } else {
            let first = args[0];
            let rest = &args[1..];
            return Some(prim_call_term(ctx, sym_cons(ctx).into(), &[first, ex_list(ctx, rest, src)?], src));
        }
    }

    "vector" ex_vector<'gc>(ctx, args, src) {
        let tmp = fresh_lvar(ctx, Symbol::from_str(ctx, "vec").into());
        let len = args.len() as i32;

        let init = prim_call_term(ctx, sym_make_vector(ctx).into(), &[constant(ctx, Value::new(len)), constant(ctx, Value::new(Value::undefined()))], src);
        let mut body = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            let setv = prim_call_term(ctx, sym_vector_set(ctx).into(), &[lref(ctx, tmp), constant(ctx, Value::new(i as i32)), *arg], src);
            body.push(setv);
        }

        if body.is_empty() {
            return Some(init);
        } else {
            body.push(lref(ctx, tmp));

            return Some(let_term(
                ctx,
                LetStyle::Let,
                Array::from_slice(*ctx, &[tmp]),
                Array::from_slice(*ctx, &[init]),
                seq_from_slice(ctx, body),
                src,
            ));
        }
    }

    "tuple" ex_tuple<'gc>(ctx, args, src) {
        /*let tmp = fresh_lvar(ctx, Symbol::from_str(ctx, "tup").into());
        let len = args.len() as i32;
        let init = prim_call_term(ctx, sym_make_tuple(ctx).into(), &[constant(ctx, Value::new(len)), constant(ctx, Value::new(Value::undefined()))], src);
        let mut body = Vec::new();
        for (i, arg) in args.iter().enumerate() {
            let setv = prim_call_term(ctx, sym_tuple_set(ctx).into(), &[lref(ctx, tmp), constant(ctx, Value::new(i as i32)), *arg], src);
            body.push(setv);
        }

        if body.is_empty() {
            return Some(init);
        } else {
            body.push(lref(ctx, tmp));

            return Some(let_term(
                ctx,
                LetStyle::Let,
                Array::from_slice(*ctx, &[tmp]),
                Array::from_slice(*ctx, &[init]),
                seq_from_slice(ctx, body),
                src,
            ));
        }*/
        Some(prim_call_term(ctx, sym_tuple(ctx).into(), args, src))
    }

    "make-tuple" ex_make_tuple<'gc>(ctx, args, src) {
        if args.len() != 2 && args.len() != 1 {
            return None;
        }
        Some(prim_call_term(ctx, sym_make_tuple(ctx).into(), args, src))
    }

    "make-vector" ex_make_vector<'gc>(ctx, args, src) {
        if args.len() != 2 && args.len() != 1 {
            return None;
        }
        Some(prim_call_term(ctx, sym_make_vector(ctx).into(), args, src))
    }

    "vector?" ex_vectorp2<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }
        Some(prim_call_term(ctx, sym_vectorp(ctx).into(), args, src))
    }

    "vector-length" ex_vector_length<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }
        Some(prim_call_term(ctx, sym_vector_length(ctx).into(), args, src))
    }

    "vector-ref" ex_vector_ref<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }
        Some(prim_call_term(ctx, sym_vector_ref(ctx).into(), args, src))
    }

    "vector-set!" ex_vector_set<'gc>(ctx, args, src) {
        if args.len() != 3 {
            return None;
        }
        Some(prim_call_term(ctx, sym_vector_set(ctx).into(), args, src))
    }

    "tuple-size" ex_tuple_size<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }
        Some(prim_call_term(ctx, sym_tuple_size(ctx).into(), args, src))
    }

    "tuple-ref" ex_tuple_ref<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }
        Some(prim_call_term(ctx, sym_tuple_ref(ctx).into(), args, src))
    }

    "tuple-set!" ex_tuple_set<'gc>(ctx, args, src) {
        if args.len() != 3 {
            return None;
        }
        Some(prim_call_term(ctx, sym_tuple_set(ctx).into(), args, src))
    }

    "current-module" ex_current_module<'gc>(ctx, args, src) {
        if args.len() > 1 {
            return None;
        }
        Some(prim_call_term(ctx, sym_current_module(ctx).into(), &args, src))
    }

    "define!" ex_define<'gc>(ctx, args, src) {
        if args.len() != 2 {
            return None;
        }
        Some(prim_call_term(ctx, sym_define(ctx).into(), args, src))
    }

    "make-syntax-transformer" ex_mkstx_transformer<'gc>(_ctx, _args, _src) {
        None
    }

    "make-syntax" ex_mkstx<'gc>(ctx, args, src) {

        if args.len() != 4 {
            return None;
        }

        Some(prim_call_term(ctx, sym_make_syntax(ctx).into(), args, src))
    }

    "$winders" ex_winders<'gc>(ctx, args, src) {
        if args.len() > 1 {
            return None;
        }
        Some(prim_call_term(ctx, sym_winders(ctx).into(), args, src))
    }

    "unspecified" ex_unspecified<'gc>(ctx, args, src) {
        if !args.is_empty() {
            return None;
        }
        Some(constant(ctx,Value::unspecified()))
    }

    "unspecified?" ex_unspecifiedp<'gc>(ctx, args, src) {
        if args.len() != 1 {
            return None;
        }
        Some(prim_call_term(ctx, sym_unspecifiedp(ctx).into(), args, src))
    }
);

pub fn expand_primitives<'gc>(ctx: Context<'gc>, t: TermRef<'gc>) -> TermRef<'gc> {
    let capy_module = list!(ctx, Symbol::from_str(ctx, "capy"));
    t.pre_order(ctx, |ctx, t| match &t.kind {
        TermKind::PrimCall(name, args) => {
            let expanders = &*PRIMITIVE_EXPANDERS;

            if let Some(f) = expanders.get(ctx, name.downcast()) {
                let term = f(ctx, args.as_slice(), t.source());

                if let Some(term) = term {
                    return term;
                } else {
                    // failed expansion: reify primitive into a function call to it instead
                    let module_ref = Gc::new(
                        *ctx,
                        Term {
                            source: t.source().into(),
                            kind: TermKind::ModuleRef(capy_module, *name, true),
                        },
                    );

                    return call_term(ctx, module_ref, args.as_slice(), t.source());
                }
            } else {
                // failed expansion: reify primitive into a function call to it instead
                let module_ref = Gc::new(
                    *ctx,
                    Term {
                        source: t.source().into(),
                        kind: TermKind::ModuleRef(capy_module, *name, true),
                    },
                );

                return call_term(ctx, module_ref, args.as_slice(), t.source());
            }
        }

        _ => t,
    })
}

pub fn multi_compare<'gc>(
    ctx: Context<'gc>,
    src: Value<'gc>,
    predicate: &str,
    args: &[TermRef<'gc>],
    not: bool,
    accept_zero: bool,
) -> Option<TermRef<'gc>> {
    if args.is_empty() && !accept_zero {
        return None;
    }

    if args.is_empty() && accept_zero {
        return Some(constant(ctx, Value::new(true)));
    }

    if args.len() == 1 {
        return Some(Term::seq(
            *ctx,
            args[0],
            constant(ctx, Value::new(true)),
            src,
        ));
    }

    if args.len() == 2 {
        if not {
            return Some(Term::cond(
                *ctx,
                Term::prims(ctx, predicate, [args[0], args[1]], src),
                constant(ctx, Value::new(false)),
                constant(ctx, Value::new(true)),
                src,
            ));
        } else {
            // valid comparison, no need to expand further
            return Some(Term::prims(ctx, predicate, [args[0], args[1]], src));
        }
    }

    let scmp = ctx.intern("cmp0");
    let mut last;
    let mut current = fresh_lvar(ctx, scmp);
    let mut vars = vec![current];
    let mut result = constant(ctx, Value::new(true));
    for i in (0..args.len() - 1).rev() {
        let scmp = ctx.str(format!("cmp{}", i + 1).as_str());
        let next = fresh_lvar(ctx, scmp);
        vars.push(next);
        last = current;
        current = next;
        let params = if not {
            [lref(ctx, current), lref(ctx, last)]
        } else {
            [lref(ctx, last), lref(ctx, current)]
        };
        let cons = if not {
            constant(ctx, Value::new(false))
        } else {
            result
        };

        let alt = if not {
            result
        } else {
            constant(ctx, Value::new(false))
        };

        result = Term::cond(
            *ctx,
            Term::prims(ctx, predicate, params, src),
            cons,
            alt,
            src,
        );
    }

    let mut inits = Vec::new();
    for &arg in args {
        inits.push(arg);
    }

    result = let_term(
        ctx,
        LetStyle::Let,
        Array::from_slice(*ctx, &vars),
        Array::from_slice(*ctx, &inits),
        result,
        src,
    );

    Some(result)
}

pub fn transitive<'gc>(
    ctx: Context<'gc>,
    src: Value<'gc>,
    op: &str,
    args: &[TermRef<'gc>],
    identity: Option<Value<'gc>>,
    one: bool,
    prefix: Option<fn(Context<'gc>, Value<'gc>, TermRef<'gc>) -> TermRef<'gc>>,
) -> Option<TermRef<'gc>> {
    if args.is_empty() {
        return identity.map(|id| Some(constant(ctx, id))).unwrap_or(None);
    }

    if args.len() == 1 && one {
        return Some(prefix.map(|f| f(ctx, src, args[0])).unwrap_or(args[0]));
    }

    if args.len() == 1 && !one {
        return None;
    }

    if args.len() == 2 {
        return Some(Term::prims(ctx, op, [args[0], args[1]], src));
    } else {
        let first = args[0];
        let rest = &args[1..];
        return Some(associate_args(ctx, src, op, first, rest));
    }
}

pub fn associate_args<'gc>(
    ctx: Context<'gc>,
    src: Value<'gc>,
    op: &str,
    first_arg: TermRef<'gc>,
    args: &[TermRef<'gc>],
) -> TermRef<'gc> {
    let next = &args[1..];
    let arg = args[0];
    if next.is_empty() {
        return Term::prims(ctx, op, [first_arg, arg], src);
    } else {
        associate_args(
            ctx,
            src,
            op,
            Term::prims(ctx, op, [first_arg, arg], src),
            next,
        )
    }
}
