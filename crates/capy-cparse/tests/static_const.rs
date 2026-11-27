//! File-scope `static const` and `constexpr` objects with constant initializers: their values are
//! recorded on the declaration, converted to the declared type, and later initializers may name
//! them. The integer values are then checked against the host compiler with `_Static_assert`.

use std::process::{Command, Stdio};

use capy_cparse::consteval::ConstVal;
use capy_cparse::parse::Parser;
use capy_cparse::pp::{Config, Preprocessor};
use capy_cparse::probe::find_cc;
use capy_cparse::types::{Abi, Decl, FloatKind, IntKind, Unit, VarDecl};

const CORPUS: &str = r#"
typedef unsigned long long Flags64;
typedef unsigned int Flags32;
enum Mode { MODE_A = 1, MODE_B = 4 };
#define SHIFT 3

static const Flags64 F_NONE = 0x0000000000000000;
static const Flags64 F_READ = 0x0000000000000001;
static const Flags64 F_WRITE = 0x0000000000000002;
static const Flags64 F_RW = F_READ | F_WRITE;
static const Flags64 F_HIGH = (Flags64)1 << 40;
static const Flags32 F_WRAP = -1;
static const int SHIFTED = 1 << SHIFT;
static const enum Mode DEFAULT_MODE = MODE_B;
static const int FROM_ENUM = MODE_A | MODE_B;
static const unsigned char SMALL = 0x1ff;
static const long NEG = -(long)SHIFTED;
static const double HALF = 0.5;
static const float THIRD = 1.0f / 3.0f;
static const int FROM_FLOAT = 2.75;
static void *const NOTHING = 0;
static const char *const GREETING = "hello";
static const char NAME[] = "capy";
constexpr int CX = 6 * 7;
const int PLAIN_CONST = CX + 1;
static const int DERIVED = PLAIN_CONST * 2;

static const char *MUTABLE_PTR = "not const";
static int MUTABLE = 5;
extern const int EXTERN_NO_INIT;
static const int SIZE_OF = sizeof(Flags64);
"#;

/// Not valid C (a static initializer naming a mutable object), so kept out of the compiled corpus.
const INVALID: &str = "static const int NOT_CONSTANT = MUTABLE + 1;\n";

fn parse(src: &str) -> Parser {
    let mut pp = Preprocessor::new(Config::default());
    let toks = pp.preprocess_str("t.c", src);
    let mut parser = Parser::new(Unit::new(Abi::host_default()));
    parser.parse_translation_unit(toks);
    parser
}

fn var<'a>(p: &'a Parser, name: &str) -> &'a VarDecl {
    p.u.decls
        .iter()
        .find_map(|d| match d {
            Decl::Var(v) if &*v.name == name => Some(v),
            _ => None,
        })
        .unwrap_or_else(|| panic!("no variable {name}"))
}

fn int(p: &Parser, name: &str) -> (i128, IntKind) {
    let v = var(p, name);
    assert!(v.is_const, "{name} should be a const object");
    match &v.init {
        Some(ConstVal::Int(i, k)) => (*i, *k),
        other => panic!("{name}: expected an integer, got {other:?}"),
    }
}

#[test]
fn integer_constants_take_their_declared_type() {
    let p = parse(CORPUS);
    assert_eq!(int(&p, "F_NONE"), (0, IntKind::ULongLong));
    assert_eq!(int(&p, "F_READ"), (1, IntKind::ULongLong));
    assert_eq!(int(&p, "F_RW"), (3, IntKind::ULongLong));
    assert_eq!(int(&p, "F_HIGH"), (1 << 40, IntKind::ULongLong));
    assert_eq!(int(&p, "F_WRAP"), (0xffff_ffff, IntKind::UInt));
    assert_eq!(int(&p, "SHIFTED").0, 8);
    assert_eq!(int(&p, "DEFAULT_MODE").0, 4);
    assert_eq!(int(&p, "FROM_ENUM").0, 5);
    assert_eq!(int(&p, "SMALL"), (0xff, IntKind::UChar));
    assert_eq!(int(&p, "NEG"), (-8, IntKind::Long));
    assert_eq!(int(&p, "FROM_FLOAT").0, 2);
    assert_eq!(int(&p, "CX").0, 42);
    assert_eq!(int(&p, "PLAIN_CONST").0, 43);
    assert_eq!(int(&p, "DERIVED").0, 86);
    assert_eq!(int(&p, "SIZE_OF").0, 8);
}

#[test]
fn floats_pointers_and_strings() {
    let p = parse(CORPUS);
    assert!(
        matches!(var(&p, "HALF").init, Some(ConstVal::Float(x, FloatKind::Double)) if x == 0.5)
    );
    match var(&p, "THIRD").init {
        Some(ConstVal::Float(x, FloatKind::Float)) => assert_eq!(x, (1.0f32 / 3.0f32) as f64),
        ref other => panic!("THIRD: {other:?}"),
    }
    assert!(matches!(var(&p, "NOTHING").init, Some(ConstVal::Ptr(0))));
    assert!(var(&p, "NOTHING").is_const);
    let g = var(&p, "GREETING");
    assert!(g.is_const);
    assert!(matches!(&g.init, Some(ConstVal::Str(s)) if s == b"hello"));
    assert!(matches!(&var(&p, "NAME").init, Some(ConstVal::Str(s)) if s == b"capy"));
}

#[test]
fn non_constant_objects_are_not_constants() {
    let p = parse(&format!("{CORPUS}{INVALID}"));
    // A pointer to const is itself mutable.
    assert!(!var(&p, "MUTABLE_PTR").is_const);
    assert!(!var(&p, "MUTABLE").is_const);
    // Naming a non-const object is not a constant expression.
    assert!(var(&p, "NOT_CONSTANT").init.is_none());
    assert!(var(&p, "EXTERN_NO_INIT").init.is_none());
}

#[test]
fn a_local_shadow_is_not_the_constant() {
    let p = parse(
        "static const int K = 3;\n\
         static inline int f(void) { int K = 9; return K; }\n\
         static const int L = K + 1;\n",
    );
    assert_eq!(int(&p, "L").0, 4);
}

/// Every integer constant the parser evaluated, compared with what the compiler computes.
#[test]
fn values_agree_with_the_compiler() {
    let Some(cc) = find_cc() else {
        eprintln!("skipped: no C compiler");
        return;
    };
    let flags = ["--target=x86_64-unknown-linux-gnu"];
    let probe = Command::new(&cc)
        .args(flags)
        .args(["-std=c2x", "-fsyntax-only", "-x", "c", "-"])
        .stdin(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .and_then(|mut c| {
            use std::io::Write;
            c.stdin
                .take()
                .unwrap()
                .write_all(b"constexpr int x = 1;\n")?;
            c.wait()
        });
    if !probe.is_ok_and(|s| s.success()) {
        eprintln!("skipped: {cc} cannot compile C23 for x86_64 Linux");
        return;
    }

    let p = parse(CORPUS);
    let mut src = String::from(CORPUS);
    let mut checked = 0;
    for d in &p.u.decls {
        if let Decl::Var(VarDecl {
            name,
            init: Some(ConstVal::Int(v, _)),
            is_const: true,
            ..
        }) = d
        {
            src.push_str(&format!(
                "_Static_assert({name} == {v}LL || ({name} < 0 && {v} == (long long){name}), \"{name}\");\n"
            ));
            checked += 1;
        }
    }
    assert!(checked >= 15, "only {checked} constants were evaluated");

    let mut child = Command::new(&cc)
        .args(flags)
        .args(["-std=c2x", "-fsyntax-only", "-w", "-x", "c", "-"])
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run cc");
    {
        use std::io::Write;
        child
            .stdin
            .take()
            .unwrap()
            .write_all(src.as_bytes())
            .unwrap();
    }
    let out = child.wait_with_output().unwrap();
    assert!(
        out.status.success(),
        "compiler disagrees:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// The header that motivated this: wgpu-native declares every flag as `static const`.
#[test]
fn webgpu_flag_constants() {
    use capy_cparse::driver::{Options, process_header};
    use capy_cparse::emit::{Builder, TextEmit};
    let dir = std::path::Path::new("/opt/homebrew/include");
    if !dir.join("webgpu.h").exists() || find_cc().is_none() {
        eprintln!("skipped: no webgpu.h");
        return;
    }
    let opts = Options {
        header: "webgpu.h".into(),
        include_dirs: vec![dir.into()],
        ..Options::default()
    };
    let b = Builder::new(TextEmit);
    let out = process_header(&b, &opts).expect("webgpu.h");
    let text: String = out.unit;
    for (name, value) in [
        ("WGPUBufferUsage_Vertex", "32"),
        ("WGPUBufferUsage_CopyDst", "8"),
        ("WGPUTextureUsage_RenderAttachment", "16"),
        ("WGPUShaderStage_Fragment", "2"),
        ("WGPUColorWriteMask_All", "15"),
    ] {
        let at = text
            .find(&format!("(var \"{name}\""))
            .unwrap_or_else(|| panic!("no var {name}"));
        let entry = &text[at..at + 300.min(text.len() - at)];
        assert!(entry.contains(&format!("(int {value})")), "{name}: {entry}");
    }
    // Every flag the header spells as a hex literal, with the value read from the header text.
    let source = std::fs::read_to_string(dir.join("webgpu.h")).expect("read webgpu.h");
    let mut checked = 0;
    for line in source.lines() {
        let Some(rest) = line.strip_prefix("static const WGPU") else {
            continue;
        };
        let Some((decl, init)) = rest.split_once(" = 0x") else {
            continue;
        };
        let name = decl.split_whitespace().last().expect("a name");
        let hex = init.trim_end_matches(';').trim();
        let value = u64::from_str_radix(hex, 16).expect("a hex literal");
        let at = text
            .find(&format!("(var \"{name}\""))
            .unwrap_or_else(|| panic!("no var {name}"));
        let entry = &text[at..at + 300.min(text.len() - at)];
        assert!(entry.contains(&format!("(int {value})")), "{name}: {entry}");
        checked += 1;
    }
    assert!(checked > 20, "only {checked} flags found in webgpu.h");
}
