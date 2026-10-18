use std::{
    ffi::OsStr,
    fmt,
    io::Write,
    sync::{
        Mutex, OnceLock,
        atomic::{AtomicBool, Ordering},
    },
    time::{Duration, Instant},
};

use mmtk::{MMTKBuilder, util::options::GCTriggerSelector};

use crate::{
    heap::{MMTK, mm::MemoryManager},
    utils::{
        FormattedSize,
        flags::{self, FlagId},
    },
};

const GC_LOG_FILTER: &str = "capy::gc=trace";
const GC_TRACE_LOG_FILTER: &str = "mmtk=trace,capy::gc=trace";
/// Target of the JIT decision log (`CAPY_JIT_LOG*`).
pub(crate) const JIT_LOG_TARGET: &str = "capy::jit";
/// Target of the per-resume trace; a child of [`JIT_LOG_TARGET`] so
/// `CAPY_JIT_LOG_RESUME` can raise it alone.
pub(crate) const JIT_RESUME_LOG_TARGET: &str = "capy::jit::resume";
/// Target of inline-cache invalidation logging (`CAPY_IC_LOG`).
pub(crate) const IC_LOG_TARGET: &str = "capy::ic";
/// Target of closure-sharing decisions (`CAPY_SHARE_DUMP`).
pub(crate) const SHARE_LOG_TARGET: &str = "capy::share";
/// Target of dump-file confirmations; always on at `Info`.
pub(crate) const DUMP_LOG_TARGET: &str = "capy::dump";
/// Target of fatal boot errors; always on at `Error`.
pub(crate) const BOOT_LOG_TARGET: &str = "capy::boot";
/// Target of the GC narrative lines (summaries, trigger decisions).
pub(crate) const GC_LOG_TARGET: &str = "capy::gc";
/// Target of the `*_STATS` exit reporters, which span several modules.
pub(crate) const STATS_LOG_TARGET: &str = "capy::stats";

/// Directives the flags ask for, as `(target, level)`. They stand in for
/// `RUST_LOG` so the flags keep working on their own.
fn flag_directives() -> Vec<(&'static str, log::LevelFilter)> {
    let mut out = Vec::new();
    if flags::jit_log_resume() {
        out.push((JIT_LOG_TARGET, log::LevelFilter::Info));
        out.push((JIT_RESUME_LOG_TARGET, log::LevelFilter::Trace));
    }
    if flags::jit_log_verbose() {
        out.push((JIT_LOG_TARGET, log::LevelFilter::Debug));
    } else if flags::jit_log() {
        out.push((JIT_LOG_TARGET, log::LevelFilter::Info));
    }
    if flags::ic_log() {
        out.push((IC_LOG_TARGET, log::LevelFilter::Info));
    }
    if flags::share_dump() {
        out.push((SHARE_LOG_TARGET, log::LevelFilter::Trace));
    }
    if stats_logging_requested() {
        out.push((STATS_LOG_TARGET, log::LevelFilter::Info));
    }
    out
}

/// Any `*_STATS` exit report requested.
fn stats_logging_requested() -> bool {
    flags::jit_stats()
        || flags::template_alloc_stats()
        || flags::template_bail_stats()
        || flags::template_op_stats()
        || flags::prim_stats()
}

/// Raise the global max level to at least `level` (never lower it).
/// Emitters whose gate is known only when they fire (dump confirmations,
/// fatal boot errors) call this before logging: the logger's static
/// filters were built before their gate could be evaluated.
pub(crate) fn ensure_max_level(level: log::LevelFilter) {
    if log::max_level() < level {
        log::set_max_level(level);
    }
}
static GC_LOGGING_ENABLED: AtomicBool = AtomicBool::new(false);
static GC_LOG_STATE: OnceLock<Mutex<GcLogState>> = OnceLock::new();

#[derive(Default)]
struct GcLogState {
    collection: u64,
    pause_started_at: Option<Instant>,
    used_before: usize,
}

pub(crate) fn args_request_gc_logging<I, S>(args: I) -> bool
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let mut args = args.into_iter();
    let _program = args.next();

    for arg in args {
        let arg = arg.as_ref();
        if arg == OsStr::new("--") {
            break;
        }
        if arg == OsStr::new("--log-trace") {
            return true;
        }
    }

    false
}

fn rust_log_mentions_mmtk() -> bool {
    flags::rust_log().is_some_and(|rust_log| {
        rust_log.split(',').any(|part| {
            let part = part.trim();
            part == "mmtk" || part.starts_with("mmtk=")
        })
    })
}

fn parse_log_filters(builder: &mut env_logger::Builder) {
    // Static scaffolding first: unmatched targets are denied, so every
    // module that logs behind a runtime gate needs a directive. These run
    // before the user filters below, so an explicit `RUST_LOG` directive
    // for the same module wins; a blanket level never overrides them.
    builder.filter_module(GC_LOG_TARGET, log::LevelFilter::Trace);
    builder.filter_module("capy::heap::logging", log::LevelFilter::Info);
    builder.filter_module(DUMP_LOG_TARGET, log::LevelFilter::Info);
    builder.filter_module(BOOT_LOG_TARGET, log::LevelFilter::Error);

    let gc_trace = process_args_request_gc_logging();
    if gc_trace {
        builder.parse_filters(GC_TRACE_LOG_FILTER);
    } else if flags::is_set(FlagId::rust_log) {
        match flags::rust_log() {
            // `RUST_LOG=""`: silence everything but explicit module
            // directives, as before.
            None => {
                builder.parse_filters("mmtk=off");
            }
            Some(rust_log) => {
                let filter = if rust_log_mentions_mmtk() {
                    rust_log.to_owned()
                } else {
                    format!("{rust_log},mmtk=off")
                };
                builder.parse_filters(&filter);
            }
        }
    } else {
        builder.parse_filters(GC_LOG_FILTER);
    }

    // An explicit `RUST_LOG` replaces the flag-driven directives.
    if !flags::is_set(FlagId::rust_log) {
        for (target, level) in flag_directives() {
            builder.filter_module(target, level);
        }
    }
}

pub(crate) fn init_rust_logger() {
    let rust_log_is_set = flags::is_set(FlagId::rust_log);
    let gc_trace = process_args_request_gc_logging();
    let mmtk_explicit = rust_log_mentions_mmtk();

    let mut builder = env_logger::Builder::new();
    parse_log_filters(&mut builder);
    if !gc_trace && !mmtk_explicit {
        builder.filter_module("mmtk", log::LevelFilter::Off);
    }
    let initialized = builder
        .format(move |buf, record| {
            if !gc_trace
                && !mmtk_explicit
                && (record.target().starts_with("mmtk")
                    || record.module_path().is_some_and(|m| m.starts_with("mmtk")))
            {
                return Ok(());
            }
            writeln!(
                buf,
                "{}",
                format_log_line(record.level(), record.target(), record.args())
            )
        })
        .try_init()
        .is_ok();

    if initialized
        && !rust_log_is_set
        && !gc_logging_enabled()
        && !gc_trace
        && flag_directives().is_empty()
        && flags::profile_out().is_none()
        && flags::parse_errors().is_empty()
    {
        log::set_max_level(log::LevelFilter::Off);
    }

    if initialized && !flags::parse_errors().is_empty() {
        for error in flags::parse_errors() {
            log::warn!("{error}");
        }
        log::warn!("hint: run `capy --help-flags` to list all flags");
    }
}

pub(crate) fn mmtk_builder() -> MMTKBuilder {
    let mut builder = MMTKBuilder::new();

    if !flags::is_set(FlagId::mmtk_gc_trigger) {
        super::heuristics::set_default_max_heap_bytes(builder.options.gc_trigger.max_heap_size());
        builder.options.gc_trigger.set(GCTriggerSelector::Delegated);
    }

    if process_args_request_gc_logging() {
        set_gc_logging_enabled(true);
        builder.options.count_live_bytes_in_gc.set(true);
    }

    if !crate::heap::plans::is_allowed_plan(*builder.options.plan) {
        builder
            .options
            .plan
            .set(mmtk::util::options::PlanSelector::ConcurrentImmix);
    }

    builder
}

pub(crate) fn gc_logging_enabled() -> bool {
    GC_LOGGING_ENABLED.load(Ordering::Relaxed)
}

pub(crate) fn set_gc_logging_enabled(enabled: bool) {
    GC_LOGGING_ENABLED.store(enabled, Ordering::Relaxed);

    if enabled {
        log::set_max_level(log::LevelFilter::Trace);
    } else if !flags::is_set(FlagId::rust_log) {
        log::set_max_level(log::LevelFilter::Off);
    }
}

pub(crate) fn gc_pause_started(mmtk: &MMTK<MemoryManager>) {
    let mut state = gc_log_state().lock().expect("lock should not be poisoned");
    state.collection = state.collection.saturating_add(1);
    state.pause_started_at = Some(Instant::now());
    state.used_before = mmtk::memory_manager::used_bytes(mmtk);
}

pub(crate) fn log_gc_completed(mmtk: &MMTK<MemoryManager>) {
    let (collection, pause, used_before) = {
        let mut state = gc_log_state().lock().expect("lock should not be poisoned");
        let pause = state
            .pause_started_at
            .take()
            .map(|started_at| started_at.elapsed())
            .unwrap_or_default();
        (state.collection, pause, state.used_before)
    };

    if !gc_logging_enabled() {
        return;
    }

    let used_after = mmtk::memory_manager::used_bytes(mmtk);
    let free = mmtk::memory_manager::free_bytes(mmtk);

    log::info!(
        target: GC_LOG_TARGET,
        "{}",
        gc_summary_message(collection, pause, used_before, used_after, free)
    );

    for (space_name, stats) in mmtk::memory_manager::live_bytes_in_last_gc(mmtk) {
        log::info!(
            target: GC_LOG_TARGET,
            "{}",
            gc_space_message(space_name, stats)
        );
    }
}

fn process_args_request_gc_logging() -> bool {
    args_request_gc_logging(std::env::args_os())
}

pub(crate) fn format_log_line(
    level: log::Level,
    module: &str,
    message: impl fmt::Display,
) -> String {
    if module.is_empty() {
        format!(";; {level}: {message}")
    } else {
        format!(";; {level}({module}): {message}")
    }
}

pub(crate) fn format_gc_summary_line(
    collection: u64,
    pause: Duration,
    used_before: usize,
    used_after: usize,
    free: usize,
) -> String {
    format_log_line(
        log::Level::Info,
        GC_LOG_TARGET,
        gc_summary_message(collection, pause, used_before, used_after, free),
    )
}

fn gc_summary_message(
    collection: u64,
    pause: Duration,
    used_before: usize,
    used_after: usize,
    free: usize,
) -> String {
    let freed = used_before.saturating_sub(used_after);
    let total = used_after.saturating_add(free);
    format!(
        "GC #{}: pause={}, heap={} -> {}, freed={}, free={}, total={}",
        collection,
        format_duration(pause),
        FormattedSize(used_before),
        FormattedSize(used_after),
        FormattedSize(freed),
        FormattedSize(free),
        FormattedSize(total),
    )
}

fn gc_space_message(space_name: &str, stats: mmtk::LiveBytesStats) -> String {
    let live_percent = if stats.used_bytes == 0 {
        0.0
    } else {
        stats.live_bytes as f64 * 100.0 / stats.used_bytes as f64
    };

    format!(
        "GC space {}: live={}, used={} ({} pages, {:.1}% live)",
        space_name,
        FormattedSize(stats.live_bytes),
        FormattedSize(stats.used_bytes),
        stats.used_pages,
        live_percent,
    )
}

fn format_duration(duration: Duration) -> String {
    if duration < Duration::from_millis(1) {
        format!("{:.2}us", duration.as_nanos() as f64 / 1_000.0)
    } else if duration < Duration::from_secs(1) {
        format!("{:.2}ms", duration.as_secs_f64() * 1_000.0)
    } else {
        format!("{:.2}s", duration.as_secs_f64())
    }
}

fn gc_log_state() -> &'static Mutex<GcLogState> {
    GC_LOG_STATE.get_or_init(|| Mutex::new(GcLogState::default()))
}
