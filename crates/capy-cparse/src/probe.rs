//! Discovery of the host C compiler
use std::path::PathBuf;
use std::process::{Command, Stdio};

#[derive(Clone, Debug, Default)]
pub struct Target {
    pub cc: String,
    pub cc_version: String,
    pub triple: String,
    pub os: String,
    pub arch: String,
    /// `(name, definition-text)` for each compiler-predefined object-like or function-like macro.
    /// For function-like macros, `name` includes the parameter list: `FOO(a,b)`.
    pub macros: Vec<(String, String)>,
    pub include_dirs: Vec<PathBuf>,
    pub framework_dirs: Vec<PathBuf>,
    pub sdk_path: Option<String>,
}

impl Target {
    pub fn ptr_bits(&self) -> u32 {
        self.macro_value("__SIZEOF_POINTER__")
            .and_then(|v| v.parse::<u32>().ok())
            .map_or(64, |b| b * 8)
    }

    pub fn macro_value(&self, name: &str) -> Option<&str> {
        self.macros
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, v)| v.as_str())
    }
}

/// Finds a C compiler by trying `cc`, `clang`, then `gcc`.
pub fn find_cc() -> Option<String> {
    ["cc", "clang", "gcc"]
        .iter()
        .find(|c| run(c, &["--version"]).is_ok())
        .map(|c| c.to_string())
}

fn run(cc: &str, args: &[&str]) -> Result<(String, String), String> {
    let out = Command::new(cc)
        .args(args)
        .stdin(Stdio::null())
        .output()
        .map_err(|e| format!("cannot run {cc}: {e}"))?;
    if !out.status.success() {
        return Err(format!(
            "{cc} {args:?} failed: {}",
            String::from_utf8_lossy(&out.stderr)
        ));
    }
    Ok((
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    ))
}

/// Probes the compiler. `std` is e.g. `"c23"`, `"c11"`; pass `None` for the compiler default.
pub fn probe(cc: Option<&str>, std: Option<&str>) -> Result<Target, String> {
    let cc = match cc {
        Some(c) => c.to_string(),
        None => find_cc().ok_or("no C compiler found (pass one explicitly)")?,
    };
    let std_flag = std.map(|s| format!("-std={s}"));
    let mut base: Vec<&str> = vec!["-E", "-x", "c"];
    if let Some(f) = &std_flag {
        base.push(f);
    }

    let cc_version = run(&cc, &["--version"])?
        .0
        .lines()
        .next()
        .unwrap_or("")
        .to_string();
    let triple = run(&cc, &["-dumpmachine"])
        .map(|r| r.0.trim().to_string())
        .unwrap_or_default();

    let mut dm = base.clone();
    dm.extend(["-dM", "/dev/null"]);
    let macros = parse_dm(&run(&cc, &dm)?.0);

    let mut v = base.clone();
    v.extend(["-v", "/dev/null"]);
    let (include_dirs, framework_dirs) = parse_search_dirs(&run(&cc, &v)?.1);

    let (arch, os) = split_triple(&triple, &macros);
    let sdk_path = if os == "macos" {
        run("xcrun", &["--show-sdk-path"])
            .ok()
            .map(|r| r.0.trim().to_string())
    } else {
        None
    };
    Ok(Target {
        cc,
        cc_version,
        triple,
        os,
        arch,
        macros,
        include_dirs,
        framework_dirs,
        sdk_path,
    })
}

fn parse_dm(text: &str) -> Vec<(String, String)> {
    text.lines()
        .filter_map(|l| {
            let rest = l.strip_prefix("#define ")?;
            // Function-like: the name ends at the closing paren immediately after the identifier.
            let name_end = rest
                .find(|c: char| !(c.is_alphanumeric() || c == '_' || c == '$'))
                .unwrap_or(rest.len());
            if rest[name_end..].starts_with('(') {
                let close = rest.find(')')?;
                let body = rest[close + 1..].trim_start();
                Some((rest[..=close].to_string(), body.to_string()))
            } else {
                Some((
                    rest[..name_end].to_string(),
                    rest[name_end..].trim_start().to_string(),
                ))
            }
        })
        .collect()
}

fn parse_search_dirs(stderr: &str) -> (Vec<PathBuf>, Vec<PathBuf>) {
    let (mut inc, mut fw) = (Vec::new(), Vec::new());
    let mut on = false;
    for line in stderr.lines() {
        if line.starts_with("#include <...> search starts here") {
            on = true;
        } else if line.starts_with("End of search list") {
            break;
        } else if on {
            let l = line.trim();
            if let Some(d) = l.strip_suffix(" (framework directory)") {
                fw.push(PathBuf::from(d));
            } else if !l.is_empty() {
                inc.push(PathBuf::from(l));
            }
        }
    }
    (inc, fw)
}

fn split_triple(triple: &str, macros: &[(String, String)]) -> (String, String) {
    let has = |n: &str| macros.iter().any(|(m, _)| m == n);
    let arch = match triple.split('-').next().unwrap_or("") {
        "arm64" | "aarch64" | "arm64e" => "aarch64",
        "amd64" | "x86_64" => "x86_64",
        "" => {
            if has("__aarch64__") {
                "aarch64"
            } else if has("__x86_64__") {
                "x86_64"
            } else {
                "unknown"
            }
        }
        other => other,
    };
    let os = if has("__APPLE__") {
        "macos"
    } else if has("__linux__") {
        "linux"
    } else if has("__FreeBSD__") {
        "freebsd"
    } else if has("_WIN32") {
        "windows"
    } else {
        "unknown"
    };
    (arch.to_string(), os.to_string())
}
