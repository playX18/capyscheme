//! The preprocessor: function-like macros, `#` and `##`, `__VA_ARGS__`, `#if` arithmetic,
//! include guards and `__has_include`.

use capy_cparse::pp::{Config, Preprocessor, Severity, render};

fn run(src: &str) -> String {
    let mut pp = Preprocessor::new(Config::default());
    render(&pp.preprocess_str("t.c", src))
}

/// `run` with all whitespace removed: how a token is spaced from the one before it is a property
/// of the lexer's `space` flag, which these tests do not pin down.
fn squash(src: &str) -> String {
    run(src).split_whitespace().collect()
}

fn errors(src: &str) -> usize {
    let mut pp = Preprocessor::new(Config::default());
    pp.preprocess_str("t.c", src);
    pp.diags
        .iter()
        .filter(|d| matches!(d.severity, Severity::Error))
        .count()
}

#[test]
fn object_and_function_macros() {
    let cases: &[(&str, &str)] = &[
        ("#define N 3\nN + N", "3 + 3"),
        ("#define ADD(a,b) ((a)+(b))\nADD(1,2)", "((1)+(2))"),
        (
            "#define ADD(a,b) ((a)+(b))\nADD(ADD(1,2),3)",
            "((((1)+(2)))+(3))",
        ),
        ("#define F(x) x\n#define G F(1)\nG", "1"),
        ("#define F(x) x\nF", "F"),
        ("#define EMPTY\nEMPTY x", "x"),
    ];
    for (src, want) in cases {
        assert_eq!(run(src).trim(), *want, "source: {src:?}");
    }
}

#[test]
fn stringify_and_paste() {
    assert_eq!(run("#define S(x) #x\nS(a + b)").trim(), "\"a + b\"");
    assert_eq!(run("#define C(a,b) a##b\nC(foo,bar)").trim(), "foobar");
    assert_eq!(run("#define C(a,b) a##b\nC(1,2)").trim(), "12");
    // the argument of `#` is not expanded, that of `##` neither
    assert_eq!(run("#define N 1\n#define S(x) #x\nS(N)").trim(), "\"N\"");
    // through a second macro it is
    assert_eq!(
        run("#define N 1\n#define S(x) #x\n#define XS(x) S(x)\nXS(N)").trim(),
        "\"1\""
    );
}

#[test]
fn variadic_macros() {
    assert_eq!(
        squash("#define L(f, ...) f(__VA_ARGS__)\nL(g, 1, 2, 3)"),
        "g(1,2,3)"
    );
    assert_eq!(squash("#define V(...) [__VA_ARGS__]\nV()"), "[]");
    assert_eq!(squash("#define V(...) [__VA_ARGS__]\nV(a)"), "[a]");
}

#[test]
fn conditionals_and_arithmetic() {
    let cases: &[(&str, &str)] = &[
        ("#if 1 + 2 * 3 == 7\nyes\n#else\nno\n#endif", "yes"),
        ("#if (1 << 4) == 16 && !0\nyes\n#else\nno\n#endif", "yes"),
        ("#if -1 < 0u\nyes\n#else\nno\n#endif", "no"), // unsigned comparison
        ("#if 0\na\n#elif 1\nb\n#else\nc\n#endif", "b"),
        ("#if 0\na\n#elif 0\nb\n#else\nc\n#endif", "c"),
        ("#define X 5\n#if X > 4 ? 1 : 0\nyes\n#endif", "yes"),
        ("#ifdef U\na\n#else\nb\n#endif", "b"),
        ("#define U\n#ifdef U\na\n#else\nb\n#endif", "a"),
        ("#ifndef U\na\n#endif", "a"),
        ("#if defined(U) || defined U\na\n#else\nb\n#endif", "b"),
        ("#if undefined_name == 0\na\n#else\nb\n#endif", "a"), // unknown names are 0
        ("#if 0x10 == 16 && 010 == 8\na\n#else\nb\n#endif", "a"),
    ];
    for (src, want) in cases {
        assert_eq!(run(src).trim(), *want, "source: {src:?}");
    }
}

#[test]
fn nested_and_skipped_groups() {
    // the directives of a skipped group are not run
    let src = "#if 0\n#define BAD 1\n#error no\n#if 1\nx\n#endif\n#endif\nBAD";
    assert_eq!(run(src).trim(), "BAD");
    assert_eq!(errors(src), 0);
}

#[test]
fn error_directive_is_reported() {
    assert_eq!(errors("#error stop"), 1);
    assert_eq!(errors("#if 1\n#else\n#error never\n#endif"), 0);
    assert!(errors("#endif") >= 1, "a stray #endif");
}

#[test]
fn undef_and_redefine() {
    assert_eq!(run("#define A 1\n#undef A\nA").trim(), "A");
    assert_eq!(run("#define A 1\n#undef A\n#define A 2\nA").trim(), "2");
}

#[test]
fn include_guard_and_pragma_once() {
    let dir = std::env::temp_dir().join(format!("capy-cparse-pp-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(
        dir.join("g.h"),
        "#ifndef G_H\n#define G_H\nint g;\n#endif\n",
    )
    .unwrap();
    std::fs::write(dir.join("o.h"), "#pragma once\nint o;\n").unwrap();
    let cfg = Config {
        include_dirs: vec![dir.clone()],
        ..Config::default()
    };
    let mut pp = Preprocessor::new(cfg);
    let text = render(&pp.preprocess_str(
        "t.c",
        "#include \"g.h\"\n#include \"g.h\"\n#include \"o.h\"\n#include \"o.h\"\n",
    ));
    assert_eq!(text.matches("int g").count(), 1, "{text}");
    assert_eq!(text.matches("int o").count(), 1, "{text}");
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
fn has_include() {
    let dir = std::env::temp_dir().join(format!("capy-cparse-has-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("here.h"), "").unwrap();
    let cfg = Config {
        include_dirs: vec![dir.clone()],
        ..Config::default()
    };
    let mut pp = Preprocessor::new(cfg);
    let text = render(&pp.preprocess_str(
        "t.c",
        "#if __has_include(\"here.h\")\nyes\n#endif\n#if __has_include(\"nowhere.h\")\nno\n#endif\n",
    ));
    assert_eq!(text.trim(), "yes");
    std::fs::remove_dir_all(&dir).ok();
}

#[test]
fn line_and_counter_builtins() {
    assert_eq!(squash("a\n__LINE__"), "a2");
    assert_eq!(squash("__COUNTER__ __COUNTER__"), "01");
}
