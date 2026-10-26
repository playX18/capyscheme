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
    Usize {
        min: usize,
        max: Option<usize>,
    },
    F64,
    Size,
    Enum(&'static [&'static str]),
    List(&'static [&'static str]),
    Path,
    /// Free-form string; empty means unset.
    Text,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Usize(usize),
    F64(f64),
    Word(&'static str),
    Words(Vec<&'static str>),
    Path(Option<PathBuf>),
    Text(Option<String>),
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
        // Pure: warnings stay buffered in `errors` (see `parse_errors`),
        // the logger emits them once it is up. Parsing runs before any
        // logging exists, so nothing here may write to stderr itself.
        Self::parse_from(&|name| var(name), &|name| var_os(name))
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
        FlagKind::Text => Value::Text((!raw.is_empty()).then(|| raw.to_string())),
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
    (Text) => { FlagKind::Text };
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
    ($name:ident, Text) => {
        pub fn $name() -> Option<&'static str> {
            match &value(FlagId::$name).value {
                Value::Text(t) => t.as_deref(),
                _ => unreachable!("flag `{}` is not text", stringify!($name)),
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
        "Only share a closure environment when every use of the shared variables comes before every use of the member's own captures.";

    share_dump: Bool, "CAPY_SHARE_DUMP", Some("--share-dump"), "0", "compiler",
        "Print per-site closure-sharing decisions (allocate/reuse, record size, members) and flow stats. Toggles log target `capy::share`.";

    closure_sharing: Bool, "CAPY_CLOSURE_SHARING", Some("--closure-sharing"), "1", "compiler",
        "Let closures share environment records instead of each allocating its own.";

    gcps_contify: Enum("0", "off", "none", "false", "scc", "legacy", "both", "dom+scc", "dom-then-scc", "dom", "dominator", "dominators"), "CAPY_GCPS_CONTIFY", Some("--gcps-contify"), "dom", "compiler",
        "Contification strategy: off (or 0), scc, dom, or both (dom+scc).";

    profile_passes: Bool, "CAPY_PROFILE_PASSES", Some("--profile-passes"), "0", "compiler",
        "Emit `;; PERF (capy)` phase timing lines to stderr.";

    strip_spans: Bool, "CAPY_STRIP_SPANS", Some("--strip-spans"), "0", "compiler",
        "Omit bytecode source maps from emitted FASLs (backtraces fall back to procedure metadata).";

    compile_dump: List("ir", "graph", "gcps", "ssa", "bytecode", "bc", "asm", "disassembly", "noopt", "all", "1"), "CAPY_COMPILE_DUMP", Some("--dump-compile"), "", "dump",
        "Per-compilation artifact dumps (comma-separated): ir, gcps, ssa, bytecode, noopt, or all.";

    compile_dump_noopt: Bool, "CAPY_COMPILE_DUMP_NOOPT", Some("--compile-dump-noopt"), "0", "dump",
        "Also dump the unoptimized (pre-lowering) IR.";

    compile_dump_dir: Path, "CAPY_COMPILE_DUMP_DIR", Some("--compile-dump-dir"), "", "dump",
        "Directory for CAPY_COMPILE_DUMP artifacts (fallback: CAPY_DUMP_DIR or ./capy-dumps).";

    compile_dump_limit: Usize(0, None), "CAPY_COMPILE_DUMP_LIMIT", Some("--compile-dump-limit"), "max", "dump",
        "Cap the number of compilations that dump artifacts (max = unlimited).";

    gcps_dump_contify: List("0", "off", "none", "false", "1", "on", "true", "all", "scc", "legacy", "dom", "dominator", "dominators"), "CAPY_GCPS_DUMP_CONTIFY", Some("--dump-gcps-contify"), "", "dump",
        "GCPS contification dumps: off/0, scc, dom, or all/1 for both sources.";

    gcps_dump_dir: Path, "CAPY_GCPS_DUMP_DIR", Some("--gcps-dump-dir"), "", "dump",
        "Directory for CAPY_GCPS_DUMP_CONTIFY dumps (fallback: CAPY_DUMP_DIR or ./capy-dumps).";

    gcps_dump_limit: Usize(0, None), "CAPY_GCPS_DUMP_LIMIT", Some("--gcps-dump-limit"), "max", "dump",
        "Cap the number of GCPS contification dumps (max = unlimited).";

    dump_dir: Path, "CAPY_DUMP_DIR", Some("--dump-dir"), "", "dump",
        "Shared fallback directory for all dump kinds (default: ./capy-dumps).";

    gc_heuristic: Enum("adaptive", "static", "compact", "aggressive", "passive", "generational"), "CAPY_GC_HEURISTIC", Some("--gc-heuristic"), "adaptive", "gc",
        "GC trigger heuristic: adaptive, static, compact, aggressive, passive, or generational.";

    gc_max_heap: Size, "CAPY_GC_MAX_HEAP", Some("--gc-max-heap"), "2G", "gc",
        "Maximum heap size with optional K/M/G suffix (e.g. 2G).";

    gc_init_heap: Size, "CAPY_GC_INIT_HEAP", Some("--gc-init-heap"), "64M", "gc",
        "Initial heap size with optional K/M/G suffix; the heap grows up to --gc-max-heap (0 = fixed at max).";

    gc_heap_growth_free_percent: Usize(0, Some(99)), "CAPY_GC_HEAP_GROWTH_FREE_PERCENT", Some("--gc-heap-growth-free-percent"), "50", "gc",
        "Target free share of the heap after a GC when resizing (clamped to 0-99).";

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

    gc_plan: Text, "MMTK_PLAN", Some("--gc-plan"), "", "gc",
        "MMTk GC plan: StickyImmix, ConcurrentImmix, MarkSweep, Immix, SemiSpace, GenImmix or GenCopy (default ConcurrentImmix).";

    mmtk_gc_trigger: Text, "MMTK_GC_TRIGGER", Some("--gc-trigger"), "", "gc",
        "MMTk GC trigger override; also read by MMTk itself, capy uses it for the default max heap.";

    engine: Enum("rust", "template"), "CAPY_ENGINE", Some("--engine"), "", "engine",
        "Execution engine: rust, template, or unset for the build default.";

    jit: Bool, "CAPY_JIT", Some("--jit"), "1", "jit",
        "Enable Just In Time compilation.";

    jit_threshold: Usize(0, None), "CAPY_JIT_THRESHOLD", Some("--jit-threshold"), "10", "jit",
        "Entries or back-edges before a procedure compiles.";

    jit_max_code: Usize(0, None), "CAPY_JIT_MAX_CODE", Some("--jit-max-code"), "max", "jit",
        "Total live compiled code in bytes past which tier-up stops (max = unlimited).";

    jit_max_code_per_block: Usize(0, None), "CAPY_JIT_MAX_CODE_PER_BLOCK", Some("--jit-max-code-per-block"), "max", "jit",
        "Largest single compiled code block in bytes; larger procedures stay interpreted (max = unlimited).";

    jit_limit: Usize(0, None), "CAPY_JIT_LIMIT", Some("--jit-limit"), "max", "jit",
        "Compile only the first n procedures, to bisect a miscompile (max = unlimited).";

    jit_log: Bool, "CAPY_JIT_LOG", Some("--jit-log"), "0", "jit",
        "Report compiles and returns to compiled code.";

    jit_log_verbose: Bool, "CAPY_JIT_LOG_VERBOSE", Some("--jit-log-verbose"), "0", "jit",
        "Add bulky per-procedure detail (bytecode dump, address maps, control-flow trace) to the JIT log.";

    jit_log_resume: Bool, "CAPY_JIT_LOG_RESUME", Some("--jit-log-resume"), "0", "jit",
        "Report every resume into compiled code (very verbose).";

    jit_stats: Bool, "CAPY_JIT_STATS", Some("--jit-stats"), "0", "jit",
        "Report compile, resume and slow-path counts at exit.";

    jit_dump_dir: Path, "CAPY_JIT_DUMP_DIR", Some("--jit-dump-dir"), "", "jit",
        "Write each compiled procedure's listing and ELF to this directory.";

    jit_workers: Usize(0, None), "CAPY_JIT_WORKERS", Some("--jit-workers"), "1", "jit",
        "Tier-up worker threads; 0 compiles synchronously on the requesting thread.";

    jit_tiers: Text, "CAPY_JIT_TIERS", Some("--jit-tiers"), "", "jit",
        "Comma-separated tiers allowed to compile (default: baseline).";

    jit_gdb: Bool, "CAPY_JIT_GDB", Some("--jit-gdb"), "0", "jit",
        "Register every loaded image with GDB's JIT interface (copies all image code).";

    jit_filter: Text, "CAPY_JIT_FILTER", Some("--jit-filter"), "", "jit",
        "Compile only procedures whose metadata contains this text, to bisect a miscompile.";

    perf_map: Bool, "CAPY_PERF_MAP", Some("--perf-map"), "0", "jit",
        "Append loaded image symbols to /tmp/perf-<pid>.map for perf.";

    template_alloc_stats: Bool, "CAPY_TEMPLATE_ALLOC_STATS", Some("--template-alloc-stats"), "0", "engine",
        "Count generated-code allocations (fast path vs capy_op_alloc) and report at exit.";

    template_bail_stats: Bool, "CAPY_TEMPLATE_BAIL_STATS", Some("--template-bail-stats"), "0", "engine",
        "Report bailed-opcode counts of the template interpreter at exit.";

    template_bail_oneway: Bool, "CAPY_TEMPLATE_BAIL_ONEWAY", Some("--template-bail-oneway"), "0", "engine",
        "Make capy_bail one-way: stay in the Rust engine after the first bail.";

    template_op_stats: Bool, "CAPY_TEMPLATE_OP_STATS", Some("--template-op-stats"), "0", "engine",
        "Count executed opcodes in the template interpreter (emitted at generation time) and report at exit.";

    template_bail: Text, "CAPY_TEMPLATE_BAIL", Some("--template-bail"), "", "engine",
        "Comma-separated opcode names forced to bail to the Rust engine.";

    prim_stats: Bool, "CAPY_PRIM_STATS", Some("--prim-stats"), "0", "engine",
        "Count primitive calls and report at exit.";

    fasl_debug: Bool, "CAPY_FASL_DEBUG", Some("--fasl-debug"), "0", "engine",
        "Load FASL files with debug entries.";

    profile_out: Path, "CAPY_PROFILE_OUT", Some("--profile-out"), "", "engine",
        "Arm the SIGPROF profiler at startup and write folded stacks to this file.";

    load_path: Text, "CAPY_LOAD_PATH", None, "", "engine",
        "Colon-separated directories prepended to the Scheme load path.";

    load_compiled_path: Text, "CAPY_LOAD_COMPILED_PATH", None, "", "engine",
        "Colon-separated directories prepended to the compiled-file load path.";

    ic_log: Bool, "CAPY_IC_LOG", Some("--ic-log"), "0", "engine",
        "Log generic-dispatch slot-protocol fires (inline-cache invalidations). Toggles log target `capy::ic`.";

    rust_log: Text, "RUST_LOG", Some("--rust-log"), "", "engine",
        "Log filter in env_logger syntax (e.g. info, capy=debug); mmtk stays silent unless named.";

    compile_threads: Usize(0, None), "CAPY_COMPILE_THREADS", Some("--compile-threads"), "0", "engine",
        "Background compile service threads; 0 means one per core, at most 2.";

    compile_stack_kb: Usize(0, None), "CAPY_COMPILE_STACK_KB", Some("--compile-stack-kb"), "0", "engine",
        "Stack size of compile service threads in KiB; 0 means 262144 (256 MiB).";

    workers: Usize(0, None), "CAPY_WORKERS", Some("--workers"), "0", "sched",
        "Thread scheduler worker threads; 0 means one per core.";

    thread_stack_kb: Usize(0, None), "CAPY_THREAD_STACK_KB", Some("--thread-stack-kb"), "0", "sched",
        "Thread stack size in KiB; 0 means 2048 (2 MiB).";

    thread_quantum_ms: Usize(0, None), "CAPY_THREAD_QUANTUM_MS", Some("--thread-quantum-ms"), "0", "sched",
        "Thread preemption quantum in milliseconds; 0 means 10.";

    signal_preempt: Bool, "CAPY_SIGNAL_PREEMPT", Some("--signal-preempt"), "1", "sched",
        "Send SIGURG to preempt long-running threads. Set to 0 to rely on the cooperative flag alone.";
}

#[allow(clippy::disallowed_methods)]
pub fn var(name: &str) -> Option<String> {
    std::env::var(name).ok()
}

#[allow(clippy::disallowed_methods)]
pub fn var_os(name: &str) -> Option<OsString> {
    std::env::var_os(name)
}

/// Every UTF-8 variable of the process.
#[allow(clippy::disallowed_methods)]
pub fn vars() -> Vec<(String, String)> {
    std::env::vars().collect()
}

/// Set a variable. Only sound before other threads read the environment, so
/// call it during start-up argument handling.
#[allow(clippy::disallowed_methods)]
pub unsafe fn set_var(name: impl AsRef<std::ffi::OsStr>, value: impl AsRef<std::ffi::OsStr>) {
    unsafe { std::env::set_var(name, value) }
}

pub fn home_dir() -> Option<String> {
    var("HOME")
}

/// The user's home directory
#[allow(clippy::disallowed_methods)]
pub fn home_directory() -> Option<PathBuf> {
    std::env::home_dir()
}

pub fn xdg_cache_home() -> Option<String> {
    var("XDG_CACHE_HOME")
}

pub fn local_app_data() -> Option<String> {
    var("LOCALAPPDATA")
}

pub fn app_data() -> Option<String> {
    var("APPDATA")
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
        Value::Text(None) => "(unset)".to_string(),
        Value::Text(Some(t)) => t.clone(),
    }
}

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
    let mut out = String::from(
        ";; Capy flags (env vars or CLI flags: --flag=value or --flag value;\n\
         ;; boolean flags also accept bare --flag and --no-flag).\n",
    );
    let mut previous_category: Option<&'static str> = None;
    for (index, def) in FLAG_DEFS.iter().enumerate() {
        if previous_category != Some(def.category) {
            out.push_str(&format!("\n;; [{}]\n", def.category));
            previous_category = Some(def.category);
        }
        let alias = match def.cli {
            Some(cli) if def.kind == FlagKind::Bool => {
                let bare = cli.strip_prefix("--").unwrap_or(cli);
                format!(" (alias {cli}, --no-{bare})")
            }
            Some(cli) => format!(" (alias {cli})"),
            None => String::new(),
        };
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

pub fn parse_errors() -> &'static [String] {
    FLAGS.errors.as_slice()
}

pub struct CliAlias {
    pub cli: &'static str,
    pub env: &'static str,
    pub is_bool: bool,
}

pub fn cli_aliases() -> &'static [CliAlias] {
    static ALIASES: LazyLock<Vec<CliAlias>> = LazyLock::new(|| {
        FLAG_DEFS
            .iter()
            .filter_map(|def| {
                def.cli.map(|cli| CliAlias {
                    cli,
                    env: def.env,
                    is_bool: def.kind == FlagKind::Bool,
                })
            })
            .collect()
    });
    ALIASES.as_slice()
}
