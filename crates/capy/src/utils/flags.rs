use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::sync::LazyLock;

/// One declared flag.
#[derive(Debug)]
pub struct FlagDef {
    pub env: &'static str,
    pub cli: Option<&'static str>,
    pub kind: FlagKind,
    pub default: &'static str,
    pub help: &'static str,
    pub category: &'static str,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FlagKind {
    Bool,
    Usize { min: usize, max: Option<usize> },
    F64,
    Size,
    Enum(&'static [&'static str]),
    List(&'static [&'static str]),
    Path,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Usize(usize),
    F64(f64),
    Word(&'static str),
    Words(Vec<&'static str>),
    Path(Option<PathBuf>),
}

#[derive(Debug)]
pub struct FlagValue {
    pub set: bool,
    pub value: Value,
}

#[derive(Debug)]
pub struct Flags {
    values: Vec<FlagValue>,
    errors: Vec<String>,
}

impl Flags {
    fn parse() -> Self {
        let flags = Self::parse_from(&|name| std::env::var(name).ok(), &|name| {
            std::env::var_os(name)
        });
        for error in &flags.errors {
            eprintln!(";; WARN  (capy)@flags: {error}");
        }
        if !flags.errors.is_empty() {
            eprintln!(";; hint: run `capy --help-flags` to list all flags");
        }
        flags
    }

    /// Parse every flag from the given readers (the environment in
    /// production, a fixed map in tests). Pure: no stderr, no globals.
    fn parse_from(
        get: &dyn Fn(&str) -> Option<String>,
        get_os: &dyn Fn(&str) -> Option<OsString>,
    ) -> Self {
        let mut values = Vec::with_capacity(FLAG_DEFS.len());
        let mut errors = Vec::new();
        for def in FLAG_DEFS {
            let flag_value = match def.kind {
                FlagKind::Path => match get_os(def.env) {
                    None => FlagValue {
                        set: false,
                        value: Value::Path(None),
                    },
                    Some(raw) => {
                        let path = (!raw.is_empty()).then(|| PathBuf::from(raw));
                        FlagValue {
                            set: true,
                            value: Value::Path(path),
                        }
                    }
                },
                _ => match get(def.env) {
                    None => FlagValue {
                        set: false,
                        value: default_value(def),
                    },
                    Some(raw) => {
                        let (value, warning) = parse_value(def, &raw);
                        if let Some(warning) = warning {
                            errors.push(warning);
                        }
                        FlagValue { set: true, value }
                    }
                },
            };
            values.push(flag_value);
        }
        Self { values, errors }
    }
}

fn parse_value(def: &FlagDef, raw: &str) -> (Value, Option<String>) {
    let raw = raw.trim();
    let warn = |detail: String| {
        format!(
            "{}='{}' is not valid: {detail}; using default '{}'",
            def.env, raw, def.default
        )
    };
    let value = match def.kind {
        FlagKind::Bool => match raw.to_ascii_lowercase().as_str() {
            "1" | "true" | "yes" | "on" | "all" => Value::Bool(true),
            "" | "0" | "false" | "no" | "off" | "none" => Value::Bool(false),
            other => {
                return (
                    default_value(def),
                    Some(warn(format!("expected a boolean, got '{other}'"))),
                );
            }
        },
        FlagKind::Usize { min, max } => {
            let number = match raw.to_ascii_lowercase().as_str() {
                "max" | "unlimited" => usize::MAX,
                _ => match raw.parse::<usize>() {
                    Ok(number) => number,
                    Err(_) => {
                        return (
                            default_value(def),
                            Some(warn("expected a non-negative integer".into())),
                        );
                    }
                },
            };
            Value::Usize(number.clamp(min, max.unwrap_or(usize::MAX)))
        }
        FlagKind::F64 => match raw.parse::<f64>() {
            Ok(number) => Value::F64(number),
            Err(_) => return (default_value(def), Some(warn("expected a float".into()))),
        },
        FlagKind::Size => match parse_size(raw) {
            Some(bytes) => Value::Usize(bytes),
            None => {
                return (
                    default_value(def),
                    Some(warn("expected a byte size (e.g. 2G, 512M, 1024k)".into())),
                );
            }
        },
        FlagKind::Enum(words) => {
            if raw.is_empty() {
                Value::Word("")
            } else {
                let lower = raw.to_ascii_lowercase();
                match words.iter().find(|word| **word == lower) {
                    Some(word) => Value::Word(word),
                    None => {
                        return (
                            default_value(def),
                            Some(warn(format!("expected one of {}", words.join("|")))),
                        );
                    }
                }
            }
        }
        FlagKind::List(words) => {
            let mut matched = Vec::new();
            let mut unknown = Vec::new();
            for part in raw
                .split(',')
                .map(str::trim)
                .filter(|part| !part.is_empty())
            {
                let lower = part.to_ascii_lowercase();
                match words.iter().find(|word| **word == lower) {
                    Some(word) => matched.push(*word),
                    None => unknown.push(part),
                }
            }
            if !unknown.is_empty() {
                return (
                    Value::Words(matched),
                    Some(warn(format!(
                        "unknown item(s) {} (expected any of {})",
                        unknown.join(", "),
                        words.join("|")
                    ))),
                );
            }
            Value::Words(matched)
        }
        FlagKind::Path => {
            let path = (!raw.is_empty()).then(|| PathBuf::from(raw));
            Value::Path(path)
        }
    };
    (value, None)
}

fn default_value(def: &FlagDef) -> Value {
    let (value, warning) = parse_value(def, def.default);
    assert!(
        warning.is_none(),
        "invalid default '{}' for {}: {}",
        def.default,
        def.env,
        warning.unwrap_or_default()
    );
    value
}

pub(crate) fn parse_size(value: &str) -> Option<usize> {
    let value = value.trim();
    if value.is_empty() {
        return None;
    }
    let split_at = value
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(value.len());
    let (digits, suffix) = value.split_at(split_at);
    let number = digits.parse::<usize>().ok()?;
    let multiplier = match suffix.trim().to_ascii_lowercase().as_str() {
        "" | "b" => 1,
        "k" | "kb" => 1024,
        "m" | "mb" => 1024 * 1024,
        "g" | "gb" => 1024 * 1024 * 1024,
        _ => return None,
    };
    number.checked_mul(multiplier)
}

macro_rules! __flags_kind {
    (Bool) => { FlagKind::Bool };
    (Usize($min:tt, $($rest:tt)*)) => { FlagKind::Usize { min: $min, max: $($rest)* } };
    (F64) => { FlagKind::F64 };
    (Size) => { FlagKind::Size };
    (Enum($($word:literal),+ $(,)?)) => { FlagKind::Enum(&[$($word),+]) };
    (List($($word:literal),+ $(,)?)) => { FlagKind::List(&[$($word),+]) };
    (Path) => { FlagKind::Path };
}

macro_rules! __flags_accessor {
    ($name:ident, Bool) => {
        pub fn $name() -> bool {
            match &value(FlagId::$name).value {
                Value::Bool(v) => *v,
                _ => unreachable!("flag `{}` is not boolean", stringify!($name)),
            }
        }
    };
    ($name:ident, Usize($min:tt, $($rest:tt)*)) => {
        pub fn $name() -> usize {
            match &value(FlagId::$name).value {
                Value::Usize(v) => *v,
                _ => unreachable!("flag `{}` is not an integer", stringify!($name)),
            }
        }
    };
    ($name:ident, F64) => {
        pub fn $name() -> f64 {
            match &value(FlagId::$name).value {
                Value::F64(v) => *v,
                _ => unreachable!("flag `{}` is not a float", stringify!($name)),
            }
        }
    };
    ($name:ident, Size) => {
        pub fn $name() -> usize {
            match &value(FlagId::$name).value {
                Value::Usize(v) => *v,
                _ => unreachable!("flag `{}` is not a byte size", stringify!($name)),
            }
        }
    };
    ($name:ident, Enum($($word:literal),+ $(,)?)) => {
        pub fn $name() -> &'static str {
            match &value(FlagId::$name).value {
                Value::Word(w) => w,
                _ => unreachable!("flag `{}` is not an enum", stringify!($name)),
            }
        }
    };
    ($name:ident, List($($word:literal),+ $(,)?)) => {
        pub fn $name() -> &'static [&'static str] {
            match &value(FlagId::$name).value {
                Value::Words(w) => w.as_slice(),
                _ => unreachable!("flag `{}` is not a word list", stringify!($name)),
            }
        }
    };
    ($name:ident, Path) => {
        pub fn $name() -> Option<&'static Path> {
            match &value(FlagId::$name).value {
                Value::Path(p) => p.as_deref(),
                _ => unreachable!("flag `{}` is not a path", stringify!($name)),
            }
        }
    };
}

macro_rules! define_flags {
    (
        $(
            $name:ident : $kind:ident $(($($kind_args:tt)*))?,
            $env:literal, $cli:expr, $default:literal, $cat:literal, $help:literal;
        )+
    ) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        #[repr(usize)]
        #[allow(non_camel_case_types)]
        pub enum FlagId {
            $($name),+
        }


        pub const FLAG_DEFS: &[FlagDef] = &[
            $(
                FlagDef {
                    env: $env,
                    cli: $cli,
                    kind: __flags_kind!($kind $(($($kind_args)*))?),
                    default: $default,
                    help: $help,
                    category: $cat,
                },
            )+
        ];

        static FLAGS: LazyLock<Flags> = LazyLock::new(Flags::parse);

        $(__flags_accessor!($name, $kind $(($($kind_args)*))?);)+


        pub fn value(id: FlagId) -> &'static FlagValue {
            &FLAGS.values[id as usize]
        }

        pub fn is_set(id: FlagId) -> bool {
            value(id).set
        }
    };
}

define_flags! {
    share_min_overlap: Usize(1, None), "CAPY_SHARE_MIN_OVERLAP", Some("--share-min-overlap"), "2", "compiler",
        "Min record-resident free vars per member for a site to join a shared env record.";

    share_chain_cost: Usize(1, None), "CAPY_SHARE_CHAIN_COST", Some("--share-chain-cost"), "3", "compiler",
        "Cross-site chain-head record cost in words.";

    share_order: Bool, "CAPY_SHARE_ORDER", Some("--share-order"), "0", "compiler",
        "Restore the FCO stage-ordering gate for closure-sharing reuse sites.";

    share_dump: Bool, "CAPY_SHARE_DUMP", Some("--share-dump"), "0", "compiler",
        "Print per-site closure-sharing decisions (allocate/reuse, record size, members) and flow stats.";

    closure_sharing: Bool, "CAPY_CLOSURE_SHARING", Some("--closure-sharing"), "1", "compiler",
        "Enable shared closure environments (EnvRecord). Set to 0 for the no-sharing baseline.";

    gcps_contify: Enum("0", "off", "none", "false", "scc", "legacy", "both", "dom+scc", "dom-then-scc", "dom", "dominator", "dominators"), "CAPY_GCPS_CONTIFY", Some("--gcps-contify"), "dom", "compiler",
        "Contification strategy: off (or 0), scc, dom, or both (dom+scc).";

    profile_passes: Bool, "CAPY_PROFILE_PASSES", Some("--profile-passes"), "0", "compiler",
        "Emit `;; PERF (capy)` phase timing lines to stderr.";

    compile_dump: List("ir", "graph", "gcps", "ssa", "clif", "cranelift", "asm", "disassembly", "noopt", "all", "1"), "CAPY_COMPILE_DUMP", Some("--dump-compile"), "", "dump",
        "Per-compilation artifact dumps (comma-separated): ir, gcps, ssa, clif, asm, noopt, or all.";

    compile_dump_noopt: Bool, "CAPY_COMPILE_DUMP_NOOPT", None, "0", "dump",
        "Also dump the unoptimized (pre-lowering) IR.";

    compile_dump_dir: Path, "CAPY_COMPILE_DUMP_DIR", None, "", "dump",
        "Directory for CAPY_COMPILE_DUMP artifacts (fallback: CAPY_DUMP_DIR or ./capy-dumps).";

    compile_dump_limit: Usize(0, None), "CAPY_COMPILE_DUMP_LIMIT", None, "max", "dump",
        "Cap the number of compilations that dump artifacts (max = unlimited).";

    sbbv_dump: List("1", "all", "pre", "expand", "specialize", "pre-expand", "post-expand", "post-specialize", "post-finish"), "CAPY_SBBV_DUMP", Some("--dump-sbbv"), "", "dump",
        "SBBV CFG stage dumps: 1 or all for every stage, else a comma-separated stage list (pre, expand, specialize, pre-expand, post-expand, post-specialize, post-finish).";

    sbbv_dump_dir: Path, "CAPY_SBBV_DUMP_DIR", None, "", "dump",
        "Directory for CAPY_SBBV_DUMP dumps (fallback: CAPY_DUMP_DIR or ./capy-dumps).";

    sbbv_dump_limit: Usize(0, None), "CAPY_SBBV_DUMP_LIMIT", None, "max", "dump",
        "Cap the number of SBBV stage dumps (max = unlimited).";

    sbbv_version_limit: Usize(0, None), "CAPY_SBBV_VERSION_LIMIT", None, "2", "compiler",
        "SBBV specialization version limit; 0 disables SBBV.";

    gcps_dump_contify: List("0", "off", "none", "false", "1", "on", "true", "all", "scc", "legacy", "dom", "dominator", "dominators"), "CAPY_GCPS_DUMP_CONTIFY", Some("--dump-gcps-contify"), "", "dump",
        "GCPS contification dumps: off/0, scc, dom, or all/1 for both sources.";

    gcps_dump_dir: Path, "CAPY_GCPS_DUMP_DIR", None, "", "dump",
        "Directory for CAPY_GCPS_DUMP_CONTIFY dumps (fallback: CAPY_DUMP_DIR or ./capy-dumps).";

    gcps_dump_limit: Usize(0, None), "CAPY_GCPS_DUMP_LIMIT", None, "max", "dump",
        "Cap the number of GCPS contification dumps (max = unlimited).";

    dump_dir: Path, "CAPY_DUMP_DIR", Some("--dump-dir"), "", "dump",
        "Shared fallback directory for all dump kinds (default: ./capy-dumps).";

    gc_heuristic: Enum("adaptive", "static", "compact", "aggressive", "passive"), "CAPY_GC_HEURISTIC", Some("--gc-heuristic"), "adaptive", "gc",
        "GC trigger heuristic: adaptive, static, compact, aggressive, or passive.";

    gc_max_heap: Size, "CAPY_GC_MAX_HEAP", Some("--gc-max-heap"), "2G", "gc",
        "Maximum heap size with optional K/M/G suffix (e.g. 2G).";

    gc_min_free_percent: Usize(0, Some(100)), "CAPY_GC_MIN_FREE_PERCENT", Some("--gc-min-free-percent"), "10", "gc",
        "Minimum free heap threshold, percent of the heap (clamped to 0-100).";

    gc_init_free_percent: Usize(0, Some(100)), "CAPY_GC_INIT_FREE_PERCENT", Some("--gc-init-free-percent"), "70", "gc",
        "Initial free heap threshold, percent of the heap (clamped to 0-100).";

    gc_allocation_threshold_percent: Usize(0, Some(100)), "CAPY_GC_ALLOCATION_THRESHOLD_PERCENT", Some("--gc-allocation-threshold-percent"), "0", "gc",
        "Allocation-since-GC threshold, percent of the heap (compact mode; clamped to 0-100).";

    gc_alloc_spike_percent: Usize(0, Some(100)), "CAPY_GC_ALLOC_SPIKE_PERCENT", Some("--gc-alloc-spike-percent"), "5", "gc",
        "Allocation spike detection factor, percent (clamped to 0-100).";

    gc_learning_steps: Usize(0, None), "CAPY_GC_LEARNING_STEPS", Some("--gc-learning-steps"), "5", "gc",
        "History length used by the adaptive heuristic.";

    gc_adaptive_confidence: F64, "CAPY_GC_ADAPTIVE_CONFIDENCE", Some("--gc-adaptive-confidence"), "1.8", "gc",
        "Confidence multiplier for the adaptive heuristic.";

    gc_adaptive_spike_threshold: F64, "CAPY_GC_ADAPTIVE_SPIKE_THRESHOLD", Some("--gc-adaptive-spike-threshold"), "1.8", "gc",
        "Spike threshold for the adaptive heuristic.";

    gc_acceleration_sample_period_ms: Usize(0, None), "CAPY_GC_ACCELERATION_SAMPLE_PERIOD_MS", Some("--gc-acceleration-sample-period-ms"), "15", "gc",
        "Acceleration sampling period in milliseconds.";

    gc_acceleration_sample_count: Usize(0, None), "CAPY_GC_ACCELERATION_SAMPLE_COUNT", Some("--gc-acceleration-sample-count"), "8", "gc",
        "Number of acceleration samples.";

    gc_momentary_spike_sample_count: Usize(0, None), "CAPY_GC_MOMENTARY_SPIKE_SAMPLE_COUNT", Some("--gc-momentary-spike-sample-count"), "3", "gc",
        "Number of momentary spike samples.";

    gc_guaranteed_interval_ms: Usize(0, None), "CAPY_GC_GUARANTEED_INTERVAL_MS", Some("--gc-guaranteed-interval-ms"), "300000", "gc",
        "Force a GC at least this often; 0 = off.";

    barrier_kind: Enum("nobarrier", "objbarrier", "satbbarrier", "fieldbarrier"), "CAPY_BARRIER_KIND", Some("--barrier-kind"), "", "gc",
        "Compile-time barrier kind override (unset = the live GC plan's barrier).";
}

fn render_value(value: &Value) -> String {
    match value {
        Value::Bool(true) => "on".to_string(),
        Value::Bool(false) => "off".to_string(),
        Value::Usize(usize::MAX) => "max".to_string(),
        Value::Usize(v) => v.to_string(),
        Value::F64(v) => format!("{v}"),
        Value::Word("") => "(unset)".to_string(),
        Value::Word(w) => (*w).to_string(),
        Value::Words(w) if w.is_empty() => "(none)".to_string(),
        Value::Words(w) => w.join(","),
        Value::Path(None) => "(unset)".to_string(),
        Value::Path(Some(p)) => p.display().to_string(),
    }
}

/// Render a byte size back to the `K|M|G` shorthand for help output.
fn render_size(bytes: usize) -> String {
    for (suffix, multiplier) in [
        ("G", 1024usize.pow(3)),
        ("M", 1024usize.pow(2)),
        ("K", 1024),
    ] {
        if bytes >= multiplier && bytes % multiplier == 0 {
            return format!("{}{}", bytes / multiplier, suffix);
        }
    }
    bytes.to_string()
}

pub fn help_text() -> String {
    let mut out =
        String::from(";; Capy flags (env vars or CLI flags: --flag=value or --flag value).\n");
    let mut previous_category: Option<&'static str> = None;
    for (index, def) in FLAG_DEFS.iter().enumerate() {
        if previous_category != Some(def.category) {
            out.push_str(&format!("\n;; [{}]\n", def.category));
            previous_category = Some(def.category);
        }
        let alias = def
            .cli
            .map(|cli| format!(" (alias {cli})"))
            .unwrap_or_default();
        let current = match &FLAGS.values[index].value {
            Value::Usize(bytes) if def.kind == FlagKind::Size => render_size(*bytes),
            value => render_value(value),
        };
        out.push_str(&format!(
            ";;   {:<42} default {:<12} current {:<12}{alias}\n;;       {}\n",
            def.env, def.default, current, def.help
        ));
    }
    out
}

/// Warnings produced by invalid environment values (once per process).
pub fn parse_errors() -> &'static [String] {
    FLAGS.errors.as_slice()
}

/// `(cli_flag, env_var)` pairs feeding the pre-boot CLI flag table.
pub fn cli_aliases() -> &'static [(&'static str, &'static str)] {
    static ALIASES: LazyLock<Vec<(&'static str, &'static str)>> = LazyLock::new(|| {
        FLAG_DEFS
            .iter()
            .filter_map(|def| def.cli.map(|cli| (cli, def.env)))
            .collect()
    });
    ALIASES.as_slice()
}
