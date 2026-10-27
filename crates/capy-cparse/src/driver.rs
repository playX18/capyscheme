use crate::ast::Expr;
use crate::consteval::ConstVal;
use crate::parse::Parser;
use crate::pp::oracle::CompilerOracle;
use crate::pp::{Config, Preprocessor, Severity};
use crate::probe::{self, Target};
use crate::emit::{Builder, Emit, Ser};
use crate::token::{Punct, Token};
use crate::types::{Abi, Ident, IntKind, Type, Unit};
use std::path::PathBuf;
use std::rc::Rc;

#[derive(Clone, Debug, Default)]
pub struct Options {
    pub header: String,
    /// `#include <header>` rather than `#include "header"`.
    pub angle: bool,
    pub include_dirs: Vec<PathBuf>,
    /// `-D NAME[=BODY]`
    pub defines: Vec<(String, Option<String>)>,
    pub std: Option<String>,
    pub cc: Option<String>,
    pub inline_bodies: bool,
}

pub struct Output<V> {
    /// The `(c-unit ...)` tree.
    pub unit: V,
    pub deps: Vec<PathBuf>,
    pub errors: usize,
}

pub fn target_sexp<E: Emit>(b: &Builder<E>, t: &Target) -> E::V {
    b.list(vec![
        b.sym("target"),
        b.list(vec![b.sym("os"), b.str(&t.os)]),
        b.list(vec![b.sym("arch"), b.str(&t.arch)]),
        b.list(vec![b.sym("triple"), b.str(&t.triple)]),
        b.list(vec![b.sym("ptr-bits"), b.int(t.ptr_bits())]),
        b.list(vec![b.sym("compiler"), b.str(&t.cc)]),
        b.list(vec![b.sym("compiler-version"), b.str(&t.cc_version)]),
        b.list(vec![
            b.sym("sdk"),
            t.sdk_path.as_ref().map_or(b.bool(false), |p| b.str(p)),
        ]),
        b.list(vec![b.sym("probed"), b.bool(true)]),
    ])
}

/// Compiler facts that decide whether a cached binding is still valid.
pub fn probe_target<E: Emit>(
    b: &Builder<E>,
    cc: Option<&str>,
    std: Option<&str>,
) -> Result<E::V, String> {
    let t = probe::probe(cc, std)?;
    let dirs: Vec<E::V> = t
        .include_dirs
        .iter()
        .chain(&t.framework_dirs)
        .map(|d| b.str(d.display().to_string()))
        .collect();
    Ok(b.list(vec![
        b.sym("c-target"),
        target_sexp(b, &t),
        b.list(std::iter::once(b.sym("search-dirs")).chain(dirs).collect()),
    ]))
}

fn macro_hint<E: Emit>(
    b: &Builder<E>,
    pp: &mut Preprocessor,
    parser: &mut Parser,
    m: &crate::pp::Macro,
) -> E::V {
    // Function-like bodies stay unexpanded: parameters are plain identifiers there and
    // nested macros are resolved by name from the macro table.
    let toks: Vec<Token> = if m.params.is_some() {
        m.body.clone()
    } else {
        pp.expand_body(&m.name)
    };
    if toks.is_empty() {
        return b.list(vec![b.sym("empty")]);
    }
    if let Ok(e) = parser.expr_from_tokens(toks.clone()) {
        if m.params.is_none() {
            if let Ok(v) = parser.u.eval(&e) {
                return b.list(vec![b.sym("const"), Ser { u: &parser.u, b }.const_val(&v)]);
            }
        }
        return b.list(vec![b.sym("expr"), Ser { u: &parser.u, b }.expr(&e)]);
    }
    if m.params.is_none() {
        if let Ok(t) = parser.type_from_tokens(toks) {
            return b.list(vec![b.sym("type"), Ser { u: &parser.u, b }.ty(&t)]);
        }
    }
    b.bool(false)
}

/// Parses a function-like macro body as an expression. The parameters are declared as plain
/// variables for the duration, so `(x)` is never read as a cast to a typedef named `x`.
fn fmacro_ast(parser: &mut Parser, params: &[Rc<str>], toks: Vec<Token>) -> Result<Expr, String> {
    let depth = (parser.u.scopes.len(), parser.u.tags.len(), parser.fn_depth);
    parser.push_scope();
    let scope = parser.u.scopes.last_mut().unwrap();
    for p in params {
        scope.insert(p.clone(), Ident::Var(Type::Int(IntKind::Int)));
    }
    let r = parser
        .with_tokens(toks, |p| p.parse_assign())
        .map_err(|e| e.msg);
    // A failed parse can leave scopes open.
    parser.u.scopes.truncate(depth.0);
    parser.u.tags.truncate(depth.1);
    parser.fn_depth = depth.2;
    r
}

fn fmacro_entry<E: Emit>(
    b: &Builder<E>,
    parser: &mut Parser,
    m: &crate::pp::Macro,
    params: &[Rc<str>],
) -> E::V {
    let skip = |why: &str| b.list(vec![b.sym("fmacro-skipped"), b.str(&*m.name), b.str(why)]);
    if m.variadic {
        return skip("variadic macro");
    }
    if m.body
        .iter()
        .any(|t| t.is_punct(Punct::Hash) || t.is_punct(Punct::HashHash))
    {
        return skip("uses # or ## (stringification or token pasting)");
    }
    if m.body.iter().any(|t| &*t.text == "__VA_ARGS__") {
        return skip("refers to __VA_ARGS__");
    }
    if m.body.is_empty() {
        return skip("empty body");
    }
    let ast = match fmacro_ast(parser, params, m.body.clone()) {
        Ok(e) => e,
        Err(msg) => return skip(&format!("not a C expression: {msg}")),
    };
    match (Ser { u: &parser.u, b }).fmacro_expr(&ast, params) {
        Ok(body) => b.list(vec![
            b.sym("fmacro"),
            b.str(&*m.name),
            b.list(params.iter().map(|p| b.str(&**p)).collect()),
            body,
            b.loc(m.loc),
        ]),
        Err(why) => skip(&why),
    }
}

fn macros_sexp<E: Emit>(
    b: &Builder<E>,
    pp: &mut Preprocessor,
    parser: &mut Parser,
) -> E::V {
    let mut ms: Vec<_> = pp.macros().filter(|m| m.loc.file != 0).cloned().collect();
    ms.sort_by(|a, b| a.name.cmp(&b.name));
    let mut out = vec![b.sym("macros")];
    for m in ms {
        if let Some(params) = &m.params {
            out.push(fmacro_entry(b, parser, &m, params));
            continue;
        }
        let hint = macro_hint(b, pp, parser, &m);
        let params = match &m.params {
            Some(p) => b.list(p.iter().map(|x| b.str(&**x)).collect()),
            None => b.bool(false),
        };
        let body = b.list(m.body.iter().map(|t| b.str(&*t.text)).collect());
        out.push(b.list(vec![
            b.sym("macro"),
            b.str(&*m.name),
            params,
            b.bool(m.variadic),
            body,
            b.loc(m.loc),
            b.bool(m.from_system),
            hint,
        ]));
    }
    b.list(out)
}

fn diagnostics<E: Emit>(b: &Builder<E>, pp: &Preprocessor, u: &Unit) -> (E::V, usize) {
    let sev = |v: Severity| match v {
        Severity::Note => b.sym("note"),
        Severity::Warning => b.sym("warning"),
        Severity::Error => b.sym("error"),
    };
    let mut errors = 0;
    let mut out = vec![b.sym("diagnostics")];
    for d in &pp.diags {
        errors += (d.severity == Severity::Error) as usize;
        out.push(b.list(vec![
            sev(d.severity),
            b.str(&d.message),
            b.str(&d.file),
            b.int(d.line),
        ]));
    }
    for (v, msg, l) in &u.diags {
        errors += (*v == Severity::Error) as usize;
        let file = pp.file_name(l.file).display().to_string();
        out.push(b.list(vec![sev(*v), b.str(msg), b.str(file), b.int(l.line)]));
    }
    (b.list(out), errors)
}

pub fn process_header<E: Emit>(
    b: &Builder<E>,
    opts: &Options,
) -> Result<Output<E::V>, String> {
    let cc = opts.cc.clone().or_else(probe::find_cc);
    let target = probe::probe(cc.as_deref(), opts.std.as_deref())?;
    let cfg = Config {
        include_dirs: opts.include_dirs.clone(),
        system_dirs: target.include_dirs.clone(),
        framework_dirs: target.framework_dirs.clone(),
        predefined: target.macros.clone(),
        defines: opts.defines.clone(),
        ..Default::default()
    };
    let mut pp = Preprocessor::new(cfg);
    pp.set_oracle(Box::new(CompilerOracle::new(
        &target.cc,
        opts.std.as_deref(),
    )));
    let (open, close) = if opts.angle { ('<', '>') } else { ('"', '"') };
    let toks = pp.preprocess_str(
        "<c-include>",
        &format!("#include {open}{}{close}\n", opts.header),
    );

    let mut parser = Parser::new(Unit::new(Abi::from_target(&target)));
    parser.parse_inline_bodies = opts.inline_bodies;
    parser.parse_translation_unit(toks);

    let diag_mark = pp.diags.len();
    let macros = macros_sexp(b, &mut pp, &mut parser);
    pp.diags.truncate(diag_mark);
    let (diags, errors) = diagnostics(b, &pp, &parser.u);
    let ser = Ser { u: &parser.u, b };
    let files = b.list(
        std::iter::once(b.sym("files"))
            .chain(pp.files().iter().map(|f| b.str(f.display().to_string())))
            .collect(),
    );
    let mut parts = vec![b.sym("c-unit"), target_sexp(b, &target), files];
    parts.extend(ser.declarations());
    parts.push(macros);
    parts.extend(ser.shim_section(&opts.header, opts.angle));
    parts.push(diags);
    Ok(Output {
        unit: b.list(parts),
        deps: pp.dependencies().to_vec(),
        errors,
    })
}

impl<V> Output<V> {
    /// `(c-result (deps FILE ...) (errors N) UNIT)`
    pub fn into_result<E: Emit<V = V>>(self, b: &Builder<E>) -> V {
        let deps = b.list(
            std::iter::once(b.sym("deps"))
                .chain(self.deps.iter().map(|d| b.str(d.display().to_string())))
                .collect(),
        );
        let errors = b.list(vec![b.sym("errors"), b.int(self.errors as i128)]);
        b.list(vec![b.sym("c-result"), deps, errors, self.unit])
    }
}

pub fn eval_tokens(parser: &mut Parser, toks: Vec<Token>) -> Option<ConstVal> {
    let e: Expr = parser.expr_from_tokens(toks).ok()?;
    parser.u.eval(&e).ok()
}
