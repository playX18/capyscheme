use std::{
    collections::HashMap,
    env, fs, io,
    path::{Path, PathBuf},
};

pub type ConfigFingerprint = u64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LspConfig {
    pub root: PathBuf,
    pub source: Option<PathBuf>,
    pub load_path: Vec<PathBuf>,
    pub append_load_path: Vec<PathBuf>,
    pub compiled_load_path: Vec<PathBuf>,
    pub extensions: Vec<String>,
    pub default_module: Vec<String>,
    pub env: HashMap<String, String>,
    pub gc_env: HashMap<String, String>,
}

impl LspConfig {
    pub fn discover(document_path: Option<&Path>, workspace_root: &Path) -> io::Result<Self> {
        let start = document_path
            .and_then(Path::parent)
            .unwrap_or(workspace_root)
            .to_path_buf();
        let config_path = find_nearest_config(&start, workspace_root);
        if let Some(path) = config_path {
            let text = fs::read_to_string(&path)?;
            let root = path.parent().unwrap_or(workspace_root).to_path_buf();
            Self::parse(&text, &root, Some(path))
                .map_err(|message| io::Error::new(io::ErrorKind::InvalidData, message))
        } else {
            Ok(Self::default_for_root(workspace_root))
        }
    }

    pub fn default_for_root(root: &Path) -> Self {
        let mut config = Self {
            root: root.to_path_buf(),
            source: None,
            load_path: vec![root.to_path_buf()],
            append_load_path: Vec::new(),
            compiled_load_path: Vec::new(),
            extensions: default_extensions(),
            default_module: vec!["capy".into(), "user".into()],
            env: HashMap::new(),
            gc_env: [
                ("MMTK_PLAN", "GenImmix"),
                ("CAPY_GC_MAX_HEAP", "2G"),
                ("CAPY_GC_HEURISTIC", "compact"),
            ]
            .into_iter()
            .map(|(key, value)| (key.to_string(), value.to_string()))
            .collect(),
        };
        config.merge_env_load_path();
        config
    }

    pub fn parse(text: &str, root: &Path, source: Option<PathBuf>) -> Result<Self, String> {
        let tokens = tokenize(text)?;
        let (form, next) = parse_expr(&tokens, 0)?;
        if next != tokens.len() {
            return Err("unexpected trailing forms in lsp-config.scm".into());
        }
        let list = match form {
            Expr::List(list) => list,
            _ => return Err("lsp-config.scm must contain a (capy-lsp ...) form".into()),
        };
        if !matches!(list.first(), Some(Expr::Symbol(sym)) if sym == "capy-lsp") {
            return Err("lsp-config.scm must start with capy-lsp".into());
        }

        let mut config = Self {
            root: root.to_path_buf(),
            source,
            load_path: Vec::new(),
            append_load_path: Vec::new(),
            compiled_load_path: Vec::new(),
            extensions: default_extensions(),
            default_module: vec!["capy".into(), "user".into()],
            env: HashMap::new(),
            gc_env: HashMap::new(),
        };

        for entry in list.iter().skip(1) {
            let Expr::List(items) = entry else {
                return Err("capy-lsp entries must be lists".into());
            };
            let Some(Expr::Symbol(key)) = items.first() else {
                return Err("capy-lsp entry must start with a symbol".into());
            };
            match key.as_str() {
                "load-path" => config.load_path = parse_paths(root, &items[1..])?,
                "append-load-path" => config.append_load_path = parse_paths(root, &items[1..])?,
                "compiled-load-path" => config.compiled_load_path = parse_paths(root, &items[1..])?,
                "extensions" => config.extensions = parse_strings(&items[1..])?,
                "default-module" => config.default_module = parse_symbol_list(&items[1..])?,
                "env" => config.env = parse_env(&items[1..])?,
                "gc-options" | "gc" => config.gc_env = parse_gc_options(&items[1..])?,
                other => return Err(format!("unknown capy-lsp config key: {other}")),
            }
        }

        if config.load_path.is_empty() {
            config.load_path.push(root.to_path_buf());
        }
        config.merge_env_load_path();
        Ok(config)
    }

    pub fn effective_load_path(&self) -> Vec<PathBuf> {
        let mut paths = self.load_path.clone();
        paths.extend(self.append_load_path.clone());
        paths
    }

    pub fn load_path_env(&self) -> String {
        join_env_paths(self.effective_load_path())
    }

    pub fn compiled_load_path_env(&self) -> Option<String> {
        if self.compiled_load_path.is_empty() {
            None
        } else {
            Some(join_env_paths(self.compiled_load_path.clone()))
        }
    }

    pub fn fingerprint(&self) -> ConfigFingerprint {
        let mut hash = Fnv1a64::default();
        hash.write_path(&self.root);
        hash.write_paths(&self.effective_load_path());
        hash.write_paths(&self.compiled_load_path);
        hash.write_strings(&self.extensions);
        hash.write_strings(&self.default_module);
        hash.write_string_map(&self.env);
        hash.write_string_map(&self.gc_env);
        hash.finish()
    }

    fn merge_env_load_path(&mut self) {
        if let Ok(value) = env::var("CAPY_LOAD_PATH") {
            self.append_load_path.extend(
                value
                    .split(':')
                    .filter(|s| !s.is_empty())
                    .map(PathBuf::from),
            );
        }
    }
}

struct Fnv1a64(u64);

impl Default for Fnv1a64 {
    fn default() -> Self {
        Self(0xcbf29ce484222325)
    }
}

impl Fnv1a64 {
    fn write_byte(&mut self, byte: u8) {
        self.0 ^= byte as u64;
        self.0 = self.0.wrapping_mul(0x100000001b3);
    }

    fn write_bytes(&mut self, bytes: &[u8]) {
        for byte in bytes {
            self.write_byte(*byte);
        }
        self.write_byte(0xff);
    }

    fn write_string(&mut self, value: &str) {
        self.write_bytes(value.as_bytes());
    }

    fn write_strings(&mut self, values: &[String]) {
        self.write_usize(values.len());
        for value in values {
            self.write_string(value);
        }
    }

    fn write_string_map(&mut self, values: &HashMap<String, String>) {
        let mut entries = values.iter().collect::<Vec<_>>();
        entries.sort_by(|(left_key, _), (right_key, _)| left_key.cmp(right_key));
        self.write_usize(entries.len());
        for (key, value) in entries {
            self.write_string(key);
            self.write_string(value);
        }
    }

    fn write_path(&mut self, path: &Path) {
        self.write_string(&path.to_string_lossy());
    }

    fn write_paths(&mut self, paths: &[PathBuf]) {
        self.write_usize(paths.len());
        for path in paths {
            self.write_path(path);
        }
    }

    fn write_usize(&mut self, value: usize) {
        self.write_bytes(&value.to_le_bytes());
    }

    fn finish(self) -> u64 {
        self.0
    }
}

fn find_nearest_config(start: &Path, workspace_root: &Path) -> Option<PathBuf> {
    let mut current = start;
    loop {
        let candidate = current.join("lsp-config.scm");
        if candidate.exists() {
            return Some(candidate);
        }
        if current == workspace_root {
            return None;
        }
        current = current.parent()?;
    }
}

fn parse_paths(root: &Path, items: &[Expr]) -> Result<Vec<PathBuf>, String> {
    parse_strings(items).map(|paths| {
        paths
            .into_iter()
            .map(|path| {
                let path = PathBuf::from(path);
                if path.is_absolute() {
                    path
                } else {
                    root.join(path)
                }
            })
            .collect()
    })
}

fn parse_strings(items: &[Expr]) -> Result<Vec<String>, String> {
    items
        .iter()
        .map(|item| match item {
            Expr::String(value) => Ok(value.clone()),
            Expr::Symbol(value) => Ok(value.clone()),
            Expr::List(_) => Err("expected string or symbol".into()),
        })
        .collect()
}

fn parse_symbol_list(items: &[Expr]) -> Result<Vec<String>, String> {
    if items.len() == 1 {
        if let Expr::List(parts) = &items[0] {
            return parts
                .iter()
                .map(|item| match item {
                    Expr::Symbol(value) => Ok(value.clone()),
                    _ => Err("default-module must contain only symbols".into()),
                })
                .collect();
        }
    }
    items
        .iter()
        .map(|item| match item {
            Expr::Symbol(value) => Ok(value.clone()),
            _ => Err("default-module must contain only symbols".into()),
        })
        .collect()
}

fn parse_env(items: &[Expr]) -> Result<HashMap<String, String>, String> {
    let mut env = HashMap::new();
    for item in items {
        let Expr::List(pair) = item else {
            return Err("env entries must be (KEY VALUE) lists".into());
        };
        if pair.len() != 2 {
            return Err("env entries must contain exactly KEY and VALUE".into());
        }
        let key = expr_as_string(&pair[0])?;
        let value = expr_as_string(&pair[1])?;
        env.insert(key, value);
    }
    Ok(env)
}

fn parse_gc_options(items: &[Expr]) -> Result<HashMap<String, String>, String> {
    let mut env = HashMap::new();
    for item in items {
        let Expr::List(pair) = item else {
            return Err("gc-options entries must be (KEY VALUE) lists".into());
        };
        if pair.len() != 2 {
            return Err("gc-options entries must contain exactly KEY and VALUE".into());
        }
        let key = expr_as_string(&pair[0])?;
        let value = expr_as_string(&pair[1])?;
        let env_key = gc_option_env(&key)
            .ok_or_else(|| format!("unknown gc-options key: {key}"))?
            .to_string();
        env.insert(env_key, value);
    }
    Ok(env)
}

fn gc_option_env(key: &str) -> Option<&'static str> {
    match key {
        "plan" | "gc-plan" | "--gc-plan" | "MMTK_PLAN" => Some("MMTK_PLAN"),
        "trigger" | "gc-trigger" | "--gc-trigger" | "MMTK_GC_TRIGGER" => Some("MMTK_GC_TRIGGER"),
        "max-heap" | "gc-max-heap" | "--gc-max-heap" | "CAPY_GC_MAX_HEAP" => {
            Some("CAPY_GC_MAX_HEAP")
        }
        "heuristic" | "gc-heuristic" | "--gc-heuristic" | "CAPY_GC_HEURISTIC" => {
            Some("CAPY_GC_HEURISTIC")
        }
        "min-free-percent"
        | "gc-min-free-percent"
        | "--gc-min-free-percent"
        | "CAPY_GC_MIN_FREE_PERCENT" => Some("CAPY_GC_MIN_FREE_PERCENT"),
        "init-free-percent"
        | "gc-init-free-percent"
        | "--gc-init-free-percent"
        | "CAPY_GC_INIT_FREE_PERCENT" => Some("CAPY_GC_INIT_FREE_PERCENT"),
        "allocation-threshold-percent"
        | "gc-allocation-threshold-percent"
        | "--gc-allocation-threshold-percent"
        | "CAPY_GC_ALLOCATION_THRESHOLD_PERCENT" => Some("CAPY_GC_ALLOCATION_THRESHOLD_PERCENT"),
        "alloc-spike-percent"
        | "gc-alloc-spike-percent"
        | "--gc-alloc-spike-percent"
        | "CAPY_GC_ALLOC_SPIKE_PERCENT" => Some("CAPY_GC_ALLOC_SPIKE_PERCENT"),
        "learning-steps"
        | "gc-learning-steps"
        | "--gc-learning-steps"
        | "CAPY_GC_LEARNING_STEPS" => Some("CAPY_GC_LEARNING_STEPS"),
        "guaranteed-interval-ms"
        | "gc-guaranteed-interval-ms"
        | "--gc-guaranteed-interval-ms"
        | "CAPY_GC_GUARANTEED_INTERVAL_MS" => Some("CAPY_GC_GUARANTEED_INTERVAL_MS"),
        _ => None,
    }
}

fn expr_as_string(expr: &Expr) -> Result<String, String> {
    match expr {
        Expr::String(value) | Expr::Symbol(value) => Ok(value.clone()),
        Expr::List(_) => Err("expected string or symbol".into()),
    }
}

fn join_env_paths(paths: Vec<PathBuf>) -> String {
    paths
        .into_iter()
        .map(|path| path.to_string_lossy().into_owned())
        .collect::<Vec<_>>()
        .join(":")
}

fn default_extensions() -> Vec<String> {
    ["scm", "sls", "sld", "sps"]
        .into_iter()
        .map(str::to_string)
        .collect()
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    List(Vec<Expr>),
    Symbol(String),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    Open,
    Close,
    Symbol(String),
    String(String),
}

fn tokenize(text: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = text.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '(' => tokens.push(Token::Open),
            ')' => tokens.push(Token::Close),
            ';' => {
                for next in chars.by_ref() {
                    if next == '\n' {
                        break;
                    }
                }
            }
            '"' => {
                let mut value = String::new();
                while let Some(next) = chars.next() {
                    match next {
                        '"' => break,
                        '\\' => match chars.next() {
                            Some('n') => value.push('\n'),
                            Some('t') => value.push('\t'),
                            Some('r') => value.push('\r'),
                            Some('"') => value.push('"'),
                            Some('\\') => value.push('\\'),
                            Some(other) => value.push(other),
                            None => return Err("unterminated string escape".into()),
                        },
                        other => value.push(other),
                    }
                }
                tokens.push(Token::String(value));
            }
            ch if ch.is_whitespace() => {}
            _ => {
                let mut symbol = String::from(ch);
                while let Some(next) = chars.peek().copied() {
                    if next.is_whitespace() || next == '(' || next == ')' || next == ';' {
                        break;
                    }
                    symbol.push(next);
                    chars.next();
                }
                tokens.push(Token::Symbol(symbol));
            }
        }
    }
    Ok(tokens)
}

fn parse_expr(tokens: &[Token], pos: usize) -> Result<(Expr, usize), String> {
    match tokens.get(pos) {
        Some(Token::Open) => {
            let mut items = Vec::new();
            let mut pos = pos + 1;
            loop {
                match tokens.get(pos) {
                    Some(Token::Close) => return Ok((Expr::List(items), pos + 1)),
                    Some(_) => {
                        let (expr, next) = parse_expr(tokens, pos)?;
                        items.push(expr);
                        pos = next;
                    }
                    None => return Err("unterminated list".into()),
                }
            }
        }
        Some(Token::Close) => Err("unexpected ')'".into()),
        Some(Token::Symbol(value)) => Ok((Expr::Symbol(value.clone()), pos + 1)),
        Some(Token::String(value)) => Ok((Expr::String(value.clone()), pos + 1)),
        None => Err("empty lsp-config.scm".into()),
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use tempfile::tempdir;

    use super::LspConfig;

    #[test]
    fn parses_config_paths_relative_to_root() {
        let dir = tempdir().unwrap();
        let config = LspConfig::parse(
            r#"(capy-lsp
                (load-path "." "lib")
                (append-load-path "vendor")
                (compiled-load-path ".capy/compiled")
                (extensions "scm" "sls")
                (default-module (demo app))
                (gc-options
                  (plan StickyImmix)
                  (max-heap "1G")
                  (trigger "DynamicHeapSize:512M,2G"))
                (env ("CAPY_CUSTOM" "yes")))"#,
            dir.path(),
            None,
        )
        .unwrap();

        assert_eq!(config.load_path[0], dir.path());
        assert_eq!(config.load_path[1], dir.path().join("lib"));
        assert_eq!(config.append_load_path[0], dir.path().join("vendor"));
        assert_eq!(config.extensions, vec!["scm", "sls"]);
        assert_eq!(config.default_module, vec!["demo", "app"]);
        assert_eq!(config.env["CAPY_CUSTOM"], "yes");
        assert_eq!(config.gc_env["MMTK_PLAN"], "StickyImmix");
        assert_eq!(config.gc_env["CAPY_GC_MAX_HEAP"], "1G");
        assert_eq!(config.gc_env["MMTK_GC_TRIGGER"], "DynamicHeapSize:512M,2G");
    }

    #[test]
    fn rejects_unknown_gc_options() {
        let dir = tempdir().unwrap();
        let err = LspConfig::parse(
            r#"(capy-lsp
                (gc-options (banana "1G")))"#,
            dir.path(),
            None,
        )
        .unwrap_err();

        assert_eq!(err, "unknown gc-options key: banana");
    }

    #[test]
    fn discovers_nearest_config() {
        let dir = tempdir().unwrap();
        let project = dir.path().join("project");
        let nested = project.join("src/deeper");
        fs::create_dir_all(&nested).unwrap();
        fs::write(
            project.join("lsp-config.scm"),
            "(capy-lsp (load-path \"lib\"))",
        )
        .unwrap();
        let file = nested.join("file.scm");
        fs::write(&file, "").unwrap();

        let config = LspConfig::discover(Some(&file), dir.path()).unwrap();
        assert_eq!(config.root, project);
        assert_eq!(config.load_path, vec![config.root.join("lib")]);
    }

    #[test]
    fn fingerprint_tracks_analysis_relevant_config() {
        let dir = tempdir().unwrap();
        let mut base = LspConfig::default_for_root(dir.path());
        let same = base.clone();

        assert_eq!(base.fingerprint(), same.fingerprint());

        base.load_path.push(dir.path().join("extra"));
        assert_ne!(base.fingerprint(), same.fingerprint());

        let mut changed_default_module = same.clone();
        changed_default_module.default_module = vec!["demo".into(), "user".into()];
        assert_ne!(changed_default_module.fingerprint(), same.fingerprint());

        let mut changed_extensions = same.clone();
        changed_extensions.extensions = vec!["sld".into()];
        assert_ne!(changed_extensions.fingerprint(), same.fingerprint());

        let mut changed_env = same.clone();
        changed_env.env.insert("CAPY_FEATURE".into(), "on".into());
        assert_ne!(changed_env.fingerprint(), same.fingerprint());

        let mut changed_gc = same.clone();
        changed_gc
            .gc_env
            .insert("MMTK_PLAN".into(), "StickyImmix".into());
        assert_ne!(changed_gc.fingerprint(), same.fingerprint());
    }
}
