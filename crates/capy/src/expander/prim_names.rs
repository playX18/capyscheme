//! The names of the primitives the expander and the compiler treat specially.
//!
//! The runtime creates a variable for each of them in the `(capy)` instance at startup, in
//! every build, so this module is not gated on `bootstrap`.

use crate::runtime::MutationContext;
use crate::runtime::objectmodel::Value;
use crate::static_symbols;

macro_rules! prim_names {
    ($($sname: ident =  $name: literal)*) => {
        paste::paste! {
            static_symbols! {
                $(
                    [<SYM_ $sname: upper>] = $name
                )*
            }

            pub fn for_each_prim_name<'gc, F: FnMut(&'static str, Value<'gc>)>(ctx: MutationContext<'gc>, mut f: F) {
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

prim_names!(
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
    // The R7RS-preferred names. Libraries re-export these (sometimes renamed
    // to the R5RS names above), and the bindings are the same native
    // procedures as the primitives, so a call must compile to the primitive.
    inexact = "inexact"
    exact = "exact"
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

    // Fixnum / flonum Tier-1 (checked, ovf)
    fx_add = "fx+"
    fx_sub = "fx-"
    fx_mul = "fx*"
    fx_add_ovf = "fx+/ovf?"
    fx_sub_ovf = "fx-/ovf?"
    fx_mul_ovf = "fx*/ovf?"
    fx_lt = "fx<?"
    fx_le = "fx<=?"
    fx_gt = "fx>?"
    fx_ge = "fx>=?"
    fx_eq = "fx=?"
    fxand = "fxand"
    fxior = "fxior"
    fxxor = "fxxor"
    fxnot = "fxnot"
    fxlogand = "fxlogand"
    fxlogior = "fxlogior"
    fxlogxor = "fxlogxor"
    fx_ashl = "fxarithmetic-shift-left"
    fx_ashr = "fxarithmetic-shift-right"
    fxzerop = "fxzero?"
    fxpositivep = "fxpositive?"
    fxnegativep = "fxnegative?"
    fxoddp = "fxodd?"
    fxevenp = "fxeven?"
    fxmin = "fxmin"
    fxmax = "fxmax"
    fl_add = "fl+"
    fl_sub = "fl-"
    fl_mul = "fl*"
    fl_div = "fl/"
    fl_lt = "fl<?"
    fl_le = "fl<=?"
    fl_gt = "fl>?"
    fl_ge = "fl>=?"
    fl_eq = "fl=?"
    flzerop = "flzero?"
    flpositivep = "flpositive?"
    flnegativep = "flnegative?"
    flnanp = "flnan?"
    flinfinitep = "flinfinite?"
    flfinitep = "flfinite?"
    flmin = "flmin"
    flmax = "flmax"
    flabs = "flabs"
    flfloor = "flfloor"
    flceiling = "flceiling"
    fltruncate = "fltruncate"
    flround = "flround"
    flsin = "flsin"
    flcos = "flcos"
    fltan = "fltan"
    flexp = "flexp"
    fllog = "fllog"
    flasin = "flasin"
    flacos = "flacos"
    flsqrt = "flsqrt"
    flatan = "flatan"
    // booleans
    not = "not"

    // predicates
    fixnump = "fixnum?"
    flonump = "flonum?"
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
    reverse = "reverse"

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

    slot_ref = "slot-ref"
    slot_set = "slot-set!"



    string_length = "string-length"
    string_ref = "string-ref"
    string_set = "string-set!"

    core_hash_ref = "core-hash-ref"
    core_hash_contains = "core-hash-contains?"
    core_hash_put = "core-hash-put!"
    core_hash_set = "core-hash-set!"
    fluid_ref = "fluid-ref"
    fluid_set = "fluid-set!"

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

    bv_u8_set = "bytevector-u8-set!"
    bv_s8_ref = "bytevector-s8-ref"
    bv_s8_set = "bytevector-s8-set!"
    bv_u16_ref = "bytevector-u16-native-ref"
    bv_u16_set = "bytevector-u16-native-set!"
    bv_s16_ref = "bytevector-s16-native-ref"
    bv_s16_set = "bytevector-s16-native-set!"
    bv_u32_ref = "bytevector-u32-native-ref"
    bv_u32_set = "bytevector-u32-native-set!"
    bv_s32_ref = "bytevector-s32-native-ref"
    bv_s32_set = "bytevector-s32-native-set!"
    bv_u64_ref = "bytevector-u64-native-ref"
    bv_u64_set = "bytevector-u64-native-set!"
    bv_s64_ref = "bytevector-s64-native-ref"
    bv_s64_set = "bytevector-s64-native-set!"
    bv_f32_ref = "bytevector-ieee-single-native-ref"
    bv_f32_set = "bytevector-ieee-single-native-set!"
    bv_f64_ref = "bytevector-ieee-double-native-ref"
    bv_f64_set = "bytevector-ieee-double-native-set!"
    ptr_u8_ref = "pointer-ref/u8"
    ptr_u8_set = "pointer-set!/u8"
    ptr_s8_ref = "pointer-ref/s8"
    ptr_s8_set = "pointer-set!/s8"
    ptr_u16_ref = "pointer-ref/u16"
    ptr_u16_set = "pointer-set!/u16"
    ptr_s16_ref = "pointer-ref/s16"
    ptr_s16_set = "pointer-set!/s16"
    ptr_u32_ref = "pointer-ref/u32"
    ptr_u32_set = "pointer-set!/u32"
    ptr_s32_ref = "pointer-ref/s32"
    ptr_s32_set = "pointer-set!/s32"
    ptr_u64_ref = "pointer-ref/u64"
    ptr_u64_set = "pointer-set!/u64"
    ptr_s64_ref = "pointer-ref/s64"
    ptr_s64_set = "pointer-set!/s64"
    ptr_f32_ref = "pointer-ref/f32"
    ptr_f32_set = "pointer-set!/f32"
    ptr_f64_ref = "pointer-ref/f64"
    ptr_f64_set = "pointer-set!/f64"

    winders = "$winders"

    callcc = "call/cc"
    call_with_current_continuation = "call-with-current-continuation"
    call1cc = "call/1cc"

    unspecified = "unspecified"
    unspecifiedp = "unspecified?"
    assertion_violation = "assertion-violation"
);
