//! What the whole-program shaker of the static linker may assume about a native primitive.
//!
//! This is the one place the effects of the primitives are written down. The shaker
//! (`pkgs/std/src/capy/compiler/static/shake.scm`) decides which top-level bindings of a linked program
//! are dropped when nothing uses them, so it has to know which calls do nothing observable.
//! Scheme code reads the table with `primitive-effect`. A primitive that is not listed here
//! has an unknown effect, which the shaker treats as the worst one.
//!
//! The entries are claims about the primitives, so they must stay true when a primitive
//! changes: a primitive that starts to write somewhere, or to retain its argument, has to
//! leave its list.

/// What a call of a primitive does, when its result is not used.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrimitiveEffect {
    /// Nothing observable. It may allocate, read a mutable object, or raise on arguments of
    /// the wrong type.
    Pure,
    /// [`Pure`](Self::Pure), and the result is a new object that nothing else refers to.
    Fresh,
    /// Changes the object that is the argument with this index, and nothing else.
    Mutates(u8),
    /// Changes only what the loader and the expander do when they run, so it matters to a
    /// program only if the program can run them.
    ExpanderState,
}

use PrimitiveEffect::*;

/// Primitives that make a new object.
const FRESH: &[&str] = &[
    "cons",
    "list",
    "vector",
    "make-vector",
    "make-string",
    "string",
    "make-bytevector",
    "bytevector",
    "string-append",
    "substring",
    "string-copy",
    "vector-copy",
    "bytevector-copy",
    "list-copy",
    "make-tuple",
    "tuple",
    "make-fluid",
    "make-thread-local-fluid",
    "make-variable",
    "make-box",
    "make-core-hash-eq",
    "make-core-hash-equal",
    "make-core-hash-eqv",
    "make-core-hash-string",
    "make-core-hashtable",
    "make-weak-core-hashtable",
    "make-core-persistent-map-eq",
    "make-core-persistent-map-equal",
    "make-core-persistent-map-eqv",
    "make-class",
    "make-generic",
    "make-linklet-instance",
    "make-mutex",
    "make-weak-mapping",
    "make-ephemeron",
    "make-record-type-descriptor",
    "make-record-constructor-descriptor",
];

/// Primitives with no effect that do not necessarily make a new object.
const PURE: &[&str] = &[
    "list*",
    "cons*",
    "append",
    "reverse",
    "vector->list",
    "list->vector",
    "string->list",
    "list->string",
    "string->symbol",
    "symbol->string",
    "string->number",
    "number->string",
    "string-length",
    "vector-length",
    "bytevector-length",
    "string-ref",
    "vector-ref",
    "bytevector-u8-ref",
    "list-tail",
    "list-ref",
    "length",
    "list?",
    "car",
    "cdr",
    "caar",
    "cadr",
    "cdar",
    "cddr",
    "caddr",
    "cdddr",
    "cadddr",
    "pair?",
    "null?",
    "not",
    "eq?",
    "eqv?",
    "equal?",
    "boolean?",
    "symbol?",
    "string?",
    "char?",
    "number?",
    "integer?",
    "rational?",
    "real?",
    "complex?",
    "exact?",
    "inexact?",
    "procedure?",
    "vector?",
    "bytevector?",
    "tuple?",
    "variable?",
    "eof-object?",
    "zero?",
    "positive?",
    "negative?",
    "odd?",
    "even?",
    "+",
    "-",
    "*",
    "/",
    "=",
    "<",
    ">",
    "<=",
    ">=",
    "quotient",
    "remainder",
    "modulo",
    "abs",
    "min",
    "max",
    "expt",
    "exact",
    "inexact",
    "exact->inexact",
    "inexact->exact",
    "floor",
    "ceiling",
    "round",
    "truncate",
    "sqrt",
    "char->integer",
    "integer->char",
    "char=?",
    "char<?",
    "char>?",
    "char<=?",
    "char>=?",
    "memq",
    "memv",
    "assq",
    "assv",
    "assoc",
    "member",
    "values",
    "void",
    "unspecified",
    "unspecified?",
    "tuple-ref",
    "tuple-size",
    "fluid-ref",
    "fluid?",
    "variable-ref",
    "variable-bound?",
    "core-hash-ref",
    "core-hash-contains?",
    "core-hash-size",
    "core-hash->list",
    "core-hash->alist",
    "core-hash-copy",
    "core-hashtable?",
    "make-core-persistent-set-eq",
    "make-core-persistent-set-equal",
    "make-core-persistent-set-eqv",
    "make-syntax-transformer",
    "make-syntax",
    "make-condition",
    "class?",
    "class-of",
    "instance?",
    "instance-name",
    "symbol-hash",
    "string-hash",
    "eq-hash",
    "equal-hash",
    "string=?",
    "string<?",
    "string>?",
    "string<=?",
    "string>=?",
    "string-null?",
    "gensym",
    "generate-uid",
    "make-uuid",
    "procedure-properties",
    "procedure-property",
    "current-library-registry",
    "root-library-registry",
    "%builtin-class",
    "generic-methods",
    "method-body",
    "%make-method-body",
    "class-precedence-list",
    "char-general-category",
    "host-os",
    "host-arch",
    "host-family",
    "logand",
    "logior",
    "logxor",
    "ash",
    "hash",
    "pointer-address",
    "pointer?",
    "$continuation-marks-markss",
    "continuation-mark-set-first",
    "continuation-marks?",
    "current-continuation-marks",
    "$winders",
    "keyword->symbol",
    "keyword?",
    "fixnum-width",
    "fixnum?",
    "instance-symbols",
    "syntax-expression",
    "syntax?",
    "syntax-module",
    "syntax-sourcev",
    "$format",
    "list-transpose*",
    "%null-continuation",
    "char-numeric?",
    "bytevector-ref",
    "%utf8->string",
    "list-transpose+",
    "library-instance",
    "instance-ref",
    "instance-variable",
    "registry-resolver",
    "program-arguments",
    "bytevector->list",
    "syscall:isatty",
    "registry-envs",
    "registry-lazy-envs",
    "record-type-descriptor?",
    "record-constructor-descriptor?",
    "record?",
    "record-rtd",
    "rtd-name",
    "rtd-parent",
    "rtd-uid",
    "rtd-sealed?",
    "rtd-opaque?",
    "rtd-fields",
    "rtd-field-count",
    "rtd-printer",
    "rtd-ancestor?",
    "rcd-rtd",
    "rcd-parent",
    "rcd-protocol",
];

/// Primitives that change one of their arguments.
const MUTATING: &[(&str, u8)] = &[
    ("set-car!", 0),
    ("set-cdr!", 0),
    ("vector-set!", 0),
    ("vector-fill!", 0),
    ("string-set!", 0),
    ("bytevector-u8-set!", 0),
    ("tuple-set!", 0),
    ("fluid-set!", 0),
    ("variable-set!", 0),
    ("slot-set!", 0),
    ("instance-set!", 0),
    ("instance-slot-set!", 0),
    ("core-hash-put!", 0),
    ("core-hash-set!", 0),
    ("core-hash-delete!", 0),
    ("core-hash-remove!", 0),
    ("core-hash-clear!", 0),
    ("set-procedure-properties!", 0),
    ("set-procedure-property!", 0),
    ("set-rtd-printer!", 0),
    ("instance-ensure-variable!", 0),
    ("add-method!", 0),
    ("remove-method!", 0),
    ("%mark-boot-slot-protocol!", 0),
    ("r6rs:bytevector-copy!", 2),
];

/// Primitives that only change what the loader and the expander do.
const EXPANDER_STATE: &[&str] = &["set-registry-resolver!", "install-expander-hook!"];

/// The effect of the primitive called `name`, or `None` when nothing is known about it.
pub fn primitive_effect(name: &str) -> Option<PrimitiveEffect> {
    if FRESH.contains(&name) {
        Some(Fresh)
    } else if PURE.contains(&name) {
        Some(Pure)
    } else if let Some((_, index)) = MUTATING.iter().find(|(candidate, _)| *candidate == name) {
        Some(Mutates(*index))
    } else if EXPANDER_STATE.contains(&name) {
        Some(ExpanderState)
    } else {
        None
    }
}

/// Every primitive that has an effect listed, with it.
pub fn primitive_effects() -> impl Iterator<Item = (&'static str, PrimitiveEffect)> {
    FRESH
        .iter()
        .map(|name| (*name, Fresh))
        .chain(PURE.iter().map(|name| (*name, Pure)))
        .chain(
            MUTATING
                .iter()
                .map(|(name, index)| (*name, Mutates(*index))),
        )
        .chain(EXPANDER_STATE.iter().map(|name| (*name, ExpanderState)))
}
