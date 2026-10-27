use std::collections::HashMap;
use std::io::Write;
use std::process::{Command, Stdio};

use crate::token::Query;

pub trait FeatureOracle {
    fn query(&mut self, q: Query, arg: &str) -> i64;
}

#[derive(Default)]
pub struct StaticOracle;

const KNOWN_ATTRS: &[&str] = &[
    "aligned",
    "always_inline",
    "availability",
    "cdecl",
    "cleanup",
    "const",
    "constructor",
    "deprecated",
    "destructor",
    "format",
    "format_arg",
    "malloc",
    "mode",
    "noinline",
    "nonnull",
    "noreturn",
    "nothrow",
    "packed",
    "pure",
    "returns_nonnull",
    "sentinel",
    "unavailable",
    "unused",
    "used",
    "visibility",
    "warn_unused_result",
    "weak",
    "alias",
    "fallthrough",
    "may_alias",
    "vector_size",
    "ext_vector_type",
    "overloadable",
    "diagnose_if",
    "__aligned__",
    "__packed__",
    "__deprecated__",
    "__noreturn__",
    "__unused__",
    "__const__",
    "__pure__",
    "__format__",
    "__nonnull__",
    "__always_inline__",
    "__noinline__",
    "__visibility__",
    "__malloc__",
];

impl FeatureOracle for StaticOracle {
    fn query(&mut self, q: Query, arg: &str) -> i64 {
        let arg = arg.trim();
        match q {
            Query::HasAttribute | Query::HasCppAttribute => KNOWN_ATTRS.contains(&arg) as i64,
            Query::HasCAttribute => match arg.trim_matches('_') {
                "deprecated" | "maybe_unused" => 201904,
                "fallthrough" => 201910,
                "nodiscard" => 202003,
                "noreturn" => 202202,
                "reproducible" | "unsequenced" => 202207,
                _ => 0,
            },
            Query::HasBuiltin => arg.starts_with("__builtin_") as i64,
            _ => 0,
        }
    }
}

/// Asks the host compiler by preprocessing a one-line probe.
pub struct CompilerOracle {
    cc: String,
    args: Vec<String>,
    memo: HashMap<(Query, String), i64>,
    fallback: StaticOracle,
}

impl CompilerOracle {
    pub fn new(cc: impl Into<String>, std: Option<&str>) -> CompilerOracle {
        let mut args: Vec<String> = ["-E", "-P", "-x", "c"]
            .iter()
            .map(|s| s.to_string())
            .collect();
        if let Some(s) = std {
            args.push(format!("-std={s}"));
        }
        args.push("-".into());
        CompilerOracle {
            cc: cc.into(),
            args,
            memo: HashMap::new(),
            fallback: StaticOracle,
        }
    }

    fn ask(&self, q: Query, arg: &str) -> Option<i64> {
        let mut child = Command::new(&self.cc)
            .args(&self.args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .ok()?;
        child
            .stdin
            .take()?
            .write_all(format!("{}({arg})\n", q.name()).as_bytes())
            .ok()?;
        let out = child.wait_with_output().ok()?;
        if !out.status.success() {
            return None;
        }
        String::from_utf8_lossy(&out.stdout).trim().parse().ok()
    }
}

impl FeatureOracle for CompilerOracle {
    fn query(&mut self, q: Query, arg: &str) -> i64 {
        let key = (q, arg.trim().to_string());
        if let Some(v) = self.memo.get(&key) {
            return *v;
        }
        let v = self
            .ask(q, arg.trim())
            .unwrap_or_else(|| self.fallback.query(q, arg));
        self.memo.insert(key, v);
        v
    }
}

impl<T: FeatureOracle> FeatureOracle for std::rc::Rc<std::cell::RefCell<T>> {
    fn query(&mut self, q: Query, arg: &str) -> i64 {
        self.borrow_mut().query(q, arg)
    }
}
