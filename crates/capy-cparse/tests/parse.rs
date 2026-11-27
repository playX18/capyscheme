//! The parser and the constant evaluator, on the x86-64 LP64 ABI of `Abi::host_default`.

use capy_cparse::consteval::ConstVal;
use capy_cparse::parse::Parser;
use capy_cparse::pp::{Config, Preprocessor};
use capy_cparse::types::{Abi, Decl, FloatKind, IntKind, Type, Unit};

fn parse(src: &str) -> Parser {
    let mut pp = Preprocessor::new(Config::default());
    let toks = pp.preprocess_str("t.c", src);
    let mut parser = Parser::new(Unit::new(Abi::host_default()));
    parser.parse_translation_unit(toks);
    parser
}

fn expr(src: &str) -> Result<ConstVal, String> {
    let mut pp = Preprocessor::new(Config::default());
    let toks = pp.preprocess_str("t.c", src);
    let mut parser = Parser::new(Unit::new(Abi::host_default()));
    let e = parser.expr_from_tokens(toks)?;
    parser.u.eval(&e)
}

fn int(src: &str) -> i128 {
    match expr(src) {
        Ok(ConstVal::Int(v, _)) => v,
        other => panic!("{src}: expected an integer, got {other:?}"),
    }
}

fn typedef(parser: &Parser, name: &str) -> Type {
    parser
        .u
        .typedefs
        .get(name)
        .unwrap_or_else(|| panic!("no typedef {name}"))
        .clone()
}

fn func<'a>(parser: &'a Parser, name: &str) -> &'a capy_cparse::types::FuncDecl {
    parser
        .u
        .decls
        .iter()
        .find_map(|d| match d {
            Decl::Func(f) if &*f.name == name => Some(f),
            _ => None,
        })
        .unwrap_or_else(|| panic!("no function {name}"))
}

#[test]
fn constant_expressions() {
    assert_eq!(int("1 + 2 * 3"), 7);
    assert_eq!(int("(1 << 10) - 1"), 1023);
    assert_eq!(int("7 / 2"), 3);
    assert_eq!(int("-7 / 2"), -3);
    assert_eq!(int("-7 % 3"), -1);
    assert_eq!(int("1 ? 10 : 20"), 10);
    assert_eq!(int("0 || 5"), 1);
    assert_eq!(int("3 > 2 && 2 > 1"), 1);
    assert_eq!(int("~0 & 0xff"), 255);
    assert_eq!(int("'a'"), 97);
    assert_eq!(int("sizeof(int)"), 4);
    assert_eq!(int("sizeof(long)"), 8);
    assert_eq!(int("sizeof(char *)"), 8);
    assert_eq!(int("_Alignof(double)"), 8);
}

#[test]
fn integer_conversions_wrap() {
    assert_eq!(int("(unsigned char)257"), 1);
    assert_eq!(int("(signed char)200"), -56);
    assert_eq!(int("(unsigned)-1"), 4294967295);
    assert_eq!(int("(short)70000"), 4464);
}

#[test]
fn division_by_zero_is_an_error() {
    assert!(expr("1 / 0").is_err());
    assert!(expr("1 % 0").is_err());
}

#[test]
fn typedefs_and_declarators() {
    let p = parse(
        "typedef unsigned long size_t_;\n\
         typedef int (*cb_t)(int, char *);\n\
         typedef int arr_t[4][3];\n\
         typedef const char *str_t;\n",
    );
    assert!(matches!(typedef(&p, "size_t_"), Type::Int(IntKind::ULong)));
    match typedef(&p, "cb_t") {
        Type::Ptr(inner) => match *inner {
            Type::Func(f) => {
                assert!(matches!(f.ret, Type::Int(IntKind::Int)));
                assert_eq!(f.params.len(), 2);
                assert!(!f.variadic);
            }
            other => panic!("pointer to {other:?}"),
        },
        other => panic!("{other:?}"),
    }
    assert!(matches!(typedef(&p, "arr_t"), Type::Array(..)));
    assert!(matches!(typedef(&p, "str_t"), Type::Ptr(_)));
}

#[test]
fn function_declarations() {
    let p = parse(
        "int add(int a, int b);\n\
         void log_msg(const char *fmt, ...);\n\
         static inline int twice(int x) { return x + x; }\n\
         double (*get_cb(void))(int);\n",
    );
    let add = func(&p, "add");
    assert_eq!(add.params.len(), 2);
    assert!(!add.variadic);
    let log = func(&p, "log_msg");
    assert!(log.variadic);
    assert!(matches!(log.ret, Type::Void));
    assert!(func(&p, "twice").inline);
    // a function returning a pointer to a function
    match &func(&p, "get_cb").ret {
        Type::Ptr(inner) => match &**inner {
            Type::Func(f) => assert!(matches!(f.ret, Type::Float(FloatKind::Double))),
            other => panic!("{other:?}"),
        },
        other => panic!("{other:?}"),
    }
}

#[test]
fn structs_unions_and_anonymous_members() {
    let p = parse(
        "struct point { int x; int y; };\n\
         struct tagged { int kind; union { int i; double d; }; };\n\
         union u { char c; long l; };\n",
    );
    let by_tag = |tag: &str| {
        p.u.records
            .iter()
            .position(|r| r.tag.as_deref() == Some(tag))
            .unwrap_or_else(|| panic!("no record {tag}"))
    };
    let point = by_tag("point");
    assert_eq!(p.u.size_align(&Type::Record(point)), Some((8, 4)));
    let tagged = by_tag("tagged");
    assert_eq!(p.u.size_align(&Type::Record(tagged)), Some((16, 8)));
    // the anonymous union's members are reachable through the struct
    assert_eq!(p.u.find_field(tagged, "d").map(|(off, _)| off), Some(8));
    let u = by_tag("u");
    assert_eq!(p.u.size_align(&Type::Record(u)), Some((8, 8)));
}

#[test]
fn bit_fields_pack_into_one_unit() {
    let p = parse("struct flags { unsigned a : 3; unsigned b : 5; unsigned c : 24; };");
    let id =
        p.u.records
            .iter()
            .position(|r| r.tag.as_deref() == Some("flags"))
            .unwrap();
    assert_eq!(p.u.size_align(&Type::Record(id)), Some((4, 4)));
}

#[test]
fn enums_with_explicit_values() {
    let p = parse("enum color { RED, GREEN = 5, BLUE, BIG = 1 << 20 };");
    let e =
        p.u.enums
            .iter()
            .find(|e| e.tag.as_deref() == Some("color"))
            .unwrap();
    let consts: Vec<(&str, i128)> = e.consts.iter().map(|(n, v)| (&**n, *v)).collect();
    assert_eq!(
        consts,
        [("RED", 0), ("GREEN", 5), ("BLUE", 6), ("BIG", 1 << 20)]
    );
}

#[test]
fn enum_constants_are_usable_in_constant_expressions() {
    let mut p = parse("enum { A = 3, B = A * 2 };");
    let mut pp = Preprocessor::new(Config::default());
    let toks = pp.preprocess_str("e.c", "B + 1");
    let e = p.expr_from_tokens(toks).unwrap();
    assert_eq!(p.u.eval(&e), Ok(ConstVal::Int(7, IntKind::Int)));
}

#[test]
fn generic_selection_picks_the_matching_branch() {
    assert_eq!(int("_Generic(1, int: 10, long: 20, default: 30)"), 10);
    assert_eq!(int("_Generic(1L, int: 10, long: 20, default: 30)"), 20);
    assert_eq!(int("_Generic(1.0, int: 10, long: 20, default: 30)"), 30);
}

#[test]
fn attributes_and_pragma_pack() {
    let p = parse(
        "struct __attribute__((packed)) pk { char c; int i; };\n\
         #pragma pack(push, 1)\n\
         struct pp1 { char c; int i; };\n\
         #pragma pack(pop)\n\
         struct pp4 { char c; int i; };\n",
    );
    let size = |tag: &str| {
        let id =
            p.u.records
                .iter()
                .position(|r| r.tag.as_deref() == Some(tag))
                .unwrap();
        p.u.size_align(&Type::Record(id))
    };
    assert_eq!(size("pk"), Some((5, 1)));
    assert_eq!(size("pp1"), Some((5, 1)));
    assert_eq!(size("pp4"), Some((8, 4)));
}
