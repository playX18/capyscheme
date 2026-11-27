//! Record layout against the host C compiler: for each struct in a corpus, the sizes, alignments
//! and field offsets cparse computes are written as `_Static_assert`s and compiled with `cc`.
//! The ABI is the x86-64 LP64 one of `Abi::host_default`, so the compiler is asked for that target
//! (clang's `--target`); a compiler that cannot is skipped, with a message.

use std::process::{Command, Stdio};

use capy_cparse::parse::Parser;
use capy_cparse::pp::{Config, Preprocessor};
use capy_cparse::probe::find_cc;
use capy_cparse::types::{Abi, RecKind, Unit};

const CORPUS: &str = r#"
struct plain { char a; int b; char c; };
struct ptrs { char a; void *p; short s; };
struct nested { struct plain x; char y; };
struct arr { char a[3]; int b[2]; char c; };
struct __attribute__((packed)) packed1 { char a; int b; short c; };
#pragma pack(push, 2)
struct pack2 { char a; int b; char c; };
#pragma pack(pop)
struct aligned16 { char a; _Alignas(16) char b; };
struct bits { unsigned a : 3; unsigned b : 5; unsigned c : 30; char d; };
struct bits_zero { char a; unsigned : 0; char b; };
struct flexible { int n; char tail[]; };
struct flex_pad { char a; double tail[]; };
union u1 { char c; int i; double d; };
union u2 { char c[5]; short s; };
struct with_union { char tag; union { int i; char c[7]; } v; };
struct anon { int a; struct { char x; short y; }; int b; };
struct longs { char a; long b; long long c; unsigned long d; };
struct floats { float f; double d; char c; };
"#;

fn target_flags(cc: &str) -> Option<Vec<&'static str>> {
    for flags in [vec!["--target=x86_64-unknown-linux-gnu"], vec!["-m64"]] {
        let ok = Command::new(cc)
            .args(&flags)
            .args(["-fsyntax-only", "-x", "c", "-"])
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .and_then(|mut child| {
                use std::io::Write;
                child.stdin.take().unwrap().write_all(b"int x;\n")?;
                child.wait()
            })
            .is_ok_and(|status| status.success());
        if ok {
            return Some(flags);
        }
    }
    None
}

#[test]
fn record_layouts_match_the_host_compiler() {
    let Some(cc) = find_cc() else {
        eprintln!("skipped: no C compiler");
        return;
    };
    let Some(flags) = target_flags(&cc) else {
        eprintln!("skipped: {cc} cannot target x86-64");
        return;
    };

    let mut pp = Preprocessor::new(Config::default());
    let toks = pp.preprocess_str("layout.c", CORPUS);
    let mut parser = Parser::new(Unit::new(Abi::host_default()));
    parser.parse_translation_unit(toks);

    let mut asserts = String::new();
    let mut checked = 0;
    for record in &parser.u.records {
        let (Some(tag), Some(layout)) = (&record.tag, &record.layout) else {
            continue;
        };
        if layout.unsupported.is_some() || !record.complete {
            continue;
        }
        let keyword = if record.kind == RecKind::Union {
            "union"
        } else {
            "struct"
        };
        let ty = format!("{keyword} {tag}");
        asserts += &format!(
            "_Static_assert(sizeof({ty}) == {}, \"size of {ty}\");\n",
            layout.size
        );
        asserts += &format!(
            "_Static_assert(_Alignof({ty}) == {}, \"align of {ty}\");\n",
            layout.align
        );
        for (field, at) in record.fields.iter().zip(&layout.fields) {
            if let (Some(name), None) = (&field.name, &at.bit) {
                asserts += &format!(
                    "_Static_assert(__builtin_offsetof({ty}, {name}) == {}, \"offset of {ty}.{name}\");\n",
                    at.offset
                );
            }
        }
        checked += 1;
    }
    assert!(checked >= 10, "only {checked} records were laid out");

    let source = format!("{CORPUS}\n{asserts}");
    let mut child = Command::new(&cc)
        .args(&flags)
        .args(["-fsyntax-only", "-std=gnu11", "-x", "c", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("the compiler starts");
    {
        use std::io::Write;
        child
            .stdin
            .take()
            .unwrap()
            .write_all(source.as_bytes())
            .unwrap();
    }
    let out = child.wait_with_output().unwrap();
    assert!(
        out.status.success(),
        "{cc} disagrees with cparse's layout:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
