use super::term::*;
use lasso::Spur;
use std::collections::HashMap;
use std::sync::LazyLock;

use crate::{ast::INTERNER, ast::*};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Primitive {
    pub name: &'static str,
    /// If the primitive is impure, it means that it can throw an exception or have other side-effects.
    ///
    /// Note that allocation is not considered impure, so primitives that allocate memory are not considered impure.
    pub impure: bool,
}

impl Primitive {
    pub const fn new(name: &'static str, impure: bool) -> Self {
        Primitive { name, impure }
    }

    pub const fn is_impure(&self) -> bool {
        self.impure
    }
}

macro_rules! make_primitives {
    ($(($name: expr, $impure: expr)),*) => {
        [
            $(Primitive::new($name, $impure)),*
        ]
    };
}

static PRIMITIVE_NAMES: &[Primitive] = &make_primitives!(
    ("car", true),
    ("cdr", true),
    ("cons", false),
    ("null?", false),
    ("pair?", false),
    ("list", false),
    ("list?", false),
    ("vector", false),
    ("vector?", false),
    ("vector-ref", true),
    ("vector-set!", true),
    ("string", false),
    ("string?", false),
    ("string-length", true),
    ("string-ref", true),
    ("string-set!", true),
    ("number?", false),
    ("boolean?", false),
    ("symbol?", false),
    ("eq?", false),
    ("equal?", true),
    /* math */
    ("+", true),
    ("-", true),
    ("*", true),
    ("/", true),
    ("%", true),
    ("=", true),
    ("<", true),
    (">", true),
    ("<=", true),
    (">=", true),
    /* boxes */
    ("box", false),
    ("box?", false),
    ("unbox", true),
    ("box-set!", true),
    ("unbox-unchecked", false),
    ("set-box-unchecked!", false),
    ("define", true),
    ("gset", true),

    /* runtime & allocation */
    ("make-cell/typ8", false),
    ("make-cell/typ16", false),
    ("cell-size", false),
    ("cell-ref", false),
    ("cell-set!", false),

    // check if value is a heap allocated cell
    ("scm-cell?", false),
    // return typ8 type-code of Scheme value
    ("scm-typ8", false),
    // return typ16 type-code of Scheme value
    ("scm-typ16", false),


    /* fixnums */
    ("fx+", true),
    ("fx-", true),
    ("fx*", true),
    ("fxquotient", true),
    ("fxremainder", true),
    ("fxmodulo", true),
    ("fxabs", true),
    ("fxand", true),
    ("fxior", true),
    ("fxxor", true),
    ("fxxor", true),
    ("fxnot", true),
    ("fxlshift", true),
    ("fxrshift", true),
    ("fxpopcount", true),
    ("fxpopcount32", true),
    ("fxpopcount16", true),

    ("fx=", true),
    ("fx<", true),
    ("fx>", true),
    ("fx<=", true),
    ("fx>=", true),
    ("fxmin", true),
    ("fxmax", true),
   
    ("fx->fl", true),
    ("fl->fx", true),

    /* unsafe ops */
    ("unsafe-fx+", false),
    ("unsafe-fx-", false),
    ("unsafe-fx*", false),
    ("unsafe-fxquotient", false),
    ("unsafe-fxremainder", false),
    ("unsafe-fxmodulo", false),
    ("unsafe-fxabs", false),
    ("unsafe-fxand", false),
    ("unsafe-fxior", false),
    ("unsafe-fxxor", false),
    ("unsafe-fxxor", false),
    ("unsafe-fxnot", false),
    ("unsafe-fxlshift", false),
    ("unsafe-fxrshift", false),
    ("unsafe-fxrshift/logical", false),

    ("unsafe-fxpopcount", false),
    ("unsafe-fxpopcount32", false),
    ("unsafe-fxpopcount16", false),

    ("unsafe-fx=", false),
    ("unsafe-fx<", false),
    ("unsafe-fx>", false),
    ("unsafe-fx<=", false),
    ("unsafe-fx>=", false),
    ("unsafe-fxmin", false),
    ("unsafe-fxmax", false),


    ("unsafe-cell-ref", false),
    ("unsafe-cell-set!", false),
    ("unsafe-cell-ref/u8", false),
    ("unsafe-cell-set!/u8", false),
    ("unsafe-cell-ref/u16", false),
    ("unsafe-cell-set!/u16", false),
    ("unsafe-cell-ref/u32", false),
    ("unsafe-cell-set!/u32", false),
    ("unsafe-cell-ref/u64", false),
    ("unsafe-cell-set!/u64", false),
    /* 
        u64 ops: always unsafe and are only produced by compiler OR some very low-level code in stdlib.

        Only used for direct management of values.
    */
    ("u64", false),
    ("unsafe-u64+", false),
    ("unsafe-u64-", false),
    ("unsafe-u64*", false),
    ("unsafe-u64quotient", false),
    ("unsafe-u64remainder", false),
    ("unsafe-u64modulo", false),
    ("unsafe-u64abs", false),
    ("unsafe-u64and", false),
    ("unsafe-u64ior", false),
    ("unsafe-u64xor", false),
    ("unsafe-u64not", false),
    ("unsafe-u64lshift", false),
    ("unsafe-u64rshift", false),

    ("unsafe-u64=", false),
    ("unsafe-u64<", false),
    ("unsafe-u64>", false),
    ("unsafe-u64<=", false),
    ("unsafe-u64>=", false),

    ("apply", true),
    ("unsafe-call/cc", true)
);
pub static PRIMITIVE_SET: LazyLock<HashMap<Spur, Primitive>> = LazyLock::new(|| {
    let mut set = HashMap::new();

    for name in PRIMITIVE_NAMES.iter() {
        set.insert(INTERNER.get_or_intern(name.name), *name);
    }

    set
});

/// Given iform resolve all primitive references in it.
pub fn resolve_primitives(iform: &P<IForm>) -> P<IForm> {
    match &iform.term {
        ITerm::GRef(sym) => {
            if let Symbol::Interned(name) = &*sym.root() {
                if PRIMITIVE_SET.contains_key(name) {
                    return P(IForm {
                        span: iform.span,
                        term: ITerm::PrimRef(*name),
                    });
                }
            }

            return iform.clone();
        }

        ITerm::If(test, cons, alt) => P(IForm {
            span: iform.span,
            term: ITerm::If(
                resolve_primitives(test),
                resolve_primitives(cons),
                resolve_primitives(alt),
            ),
        }),

        ITerm::LSet(var, val) => P(IForm {
            span: iform.span,
            term: ITerm::LSet(var.clone(), resolve_primitives(val)),
        }),

        ITerm::GSet(var, val) => P(IForm {
            span: iform.span,
            term: ITerm::GSet(var.clone(), resolve_primitives(val)),
        }),

        ITerm::Seq(seq) => P(IForm {
            span: iform.span,
            term: ITerm::Seq(seq.iter().map(resolve_primitives).collect()),
        }),

        ITerm::Define(var, val) => P(IForm {
            span: iform.span,
            term: ITerm::Define(var.clone(), resolve_primitives(val)),
        }),

        ITerm::Proc(proc) => {
            let cases = proc
                .cases
                .iter()
                .map(|case| ProcCase {
                    loc: case.loc.clone(),
                    args: case.args.clone(),
                    variadic: case.variadic.clone(),
                    body: resolve_primitives(&case.body),
                })
                .collect::<Vec<_>>();
            P(IForm {
                span: iform.span,
                term: ITerm::Proc(P(Proc {
                    loc: proc.loc,
                    name: proc.name.clone(),
                    cases,
                })),
            })
        }

        ITerm::App(proc, args) => {
            let proc = resolve_primitives(proc);

            let args = args.iter().map(resolve_primitives).collect();

            if let ITerm::PrimRef(name) = &proc.term {
                return P(IForm {
                    span: iform.span,
                    term: ITerm::PrimApp(*name, args),
                });
            }
            P(IForm {
                span: iform.span,
                term: ITerm::App(proc, args),
            })
        }

        ITerm::Let(l) => {
            let initializers = l.initializers.iter().map(resolve_primitives).collect();
            let body = resolve_primitives(&l.body);

            P(IForm {
                span: iform.span,
                term: ITerm::Let(Let {
                    variables: l.variables.clone(),
                    initializers,
                    body,
                    style: l.style,
                }),
            })
        }

        ITerm::Fix(fix) => {
            let procs = fix
                .procedures
                .iter()
                .map(|proc| {
                    let cases = proc
                        .cases
                        .iter()
                        .map(|case| ProcCase {
                            loc: case.loc.clone(),
                            args: case.args.clone(),
                            variadic: case.variadic.clone(),
                            body: resolve_primitives(&case.body),
                        })
                        .collect::<Vec<_>>();
                    P(Proc {
                        loc: proc.loc,
                        name: proc.name.clone(),
                        cases,
                    })
                })
                .collect();

            P(IForm {
                span: iform.span,
                term: ITerm::Fix(Fix {
                    procedures: procs,
                    variables: fix.variables.clone(),
                    body: resolve_primitives(&fix.body),
                }),
            })
        }

        _ => iform.clone(),
    }
}

pub static IMPURE_PRIMITIVES: &[&str] = &[];
