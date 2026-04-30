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

use crate::rsgc::{MMTK, mm::MemoryManager};

const GC_LOG_FILTER: &str = "mmtk=trace,capy::gc=trace";

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

pub(crate) fn init_rust_logger() {
    let rust_log_is_set = std::env::var_os("RUST_LOG").is_some();
    let env = env_logger::Env::default().default_filter_or(GC_LOG_FILTER);
    let initialized = env_logger::Builder::from_env(env)
        .format(|buf, record| {
            writeln!(
                buf,
                "{}",
                format_log_line(record.level(), record.target(), record.args())
            )
        })
        .try_init()
        .is_ok();

    if initialized && !rust_log_is_set && !gc_logging_enabled() {
        log::set_max_level(log::LevelFilter::Off);
    }
}

pub(crate) fn mmtk_builder() -> MMTKBuilder {
    let mut builder = MMTKBuilder::new();

    if std::env::var_os("MMTK_GC_TRIGGER").is_none() {
        super::heuristics::set_default_max_heap_bytes(builder.options.gc_trigger.max_heap_size());
        builder.options.gc_trigger.set(GCTriggerSelector::Delegated);
    }

    if process_args_request_gc_logging() {
        set_gc_logging_enabled(true);
        builder.options.count_live_bytes_in_gc.set(true);
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
    } else if std::env::var_os("RUST_LOG").is_none() {
        log::set_max_level(log::LevelFilter::Off);
    }
}

pub(crate) fn gc_pause_started(mmtk: &MMTK<MemoryManager>) {
    let mut state = gc_log_state().lock().unwrap();
    state.collection = state.collection.saturating_add(1);
    state.pause_started_at = Some(Instant::now());
    state.used_before = mmtk::memory_manager::used_bytes(mmtk);
}

pub(crate) fn log_gc_completed(mmtk: &MMTK<MemoryManager>) {
    let (collection, pause, used_before) = {
        let mut state = gc_log_state().lock().unwrap();
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

    eprintln!(
        "{}",
        format_gc_summary_line(collection, pause, used_before, used_after, free)
    );

    for (space_name, stats) in mmtk::memory_manager::live_bytes_in_last_gc(mmtk) {
        eprintln!("{}", format_gc_space_line(space_name, stats));
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
    let freed = used_before.saturating_sub(used_after);
    let total = used_after.saturating_add(free);
    format_log_line(
        log::Level::Info,
        "capy::gc",
        format_args!(
            "GC #{}: pause={}, heap={} bytes ({:.2} MiB) -> {} bytes ({:.2} MiB), freed={} bytes ({:.2} MiB), free={} bytes ({:.2} MiB), total={} bytes ({:.2} MiB)",
            collection,
            format_duration(pause),
            used_before,
            bytes_to_mib(used_before),
            used_after,
            bytes_to_mib(used_after),
            freed,
            bytes_to_mib(freed),
            free,
            bytes_to_mib(free),
            total,
            bytes_to_mib(total),
        ),
    )
}

fn format_gc_space_line(space_name: &str, stats: mmtk::LiveBytesStats) -> String {
    let live_percent = if stats.used_bytes == 0 {
        0.0
    } else {
        stats.live_bytes as f64 * 100.0 / stats.used_bytes as f64
    };

    format_log_line(
        log::Level::Info,
        "capy::gc",
        format_args!(
            "GC space {}: live={} bytes, used={} bytes ({} pages, {:.1}% live)",
            space_name, stats.live_bytes, stats.used_bytes, stats.used_pages, live_percent,
        ),
    )
}

fn bytes_to_mib(bytes: usize) -> f64 {
    bytes as f64 / (1024.0 * 1024.0)
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
