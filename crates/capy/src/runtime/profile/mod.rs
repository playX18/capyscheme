//! The SIGPROF sampling profiler (P3a).
//!
//! To arm a profiler you can use two triggers: `CAPY_PROFILE_OUT`
//! set at first mutator bind (whole-run profiling), and
//! `%profile-start` (interval control). The aggregator rewrites the
//! output every drain, so a crash loses at most one interval, a final
//! dump runs at `%profile-stop` and at process exit.

pub mod capture;
pub mod maps;

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering::Relaxed};
use std::sync::{Mutex, OnceLock};

use capture::{ProfileRing, Sample};
use maps::{symbolize_caller, symbolize_running};

use crate::heap::GarbageCollector;
use crate::heap::sync::thread::Thread;
use crate::prelude::scheme;
use crate::runtime::MutationContext;

/// Whether capture is armed
pub(crate) static ARMED: AtomicBool = AtomicBool::new(false);

pub fn armed() -> bool {
    ARMED.load(Relaxed)
}

/// Sampling rate: 100Hz
const PROFILE_HZ: u64 = 100;
/// Aggregator cadence
const DRAIN_INTERVAL: std::time::Duration = std::time::Duration::from_secs(1);
const REAP_EVERY_DRAINS: u64 = 30;

static OUT_PATH: OnceLock<PathBuf> = OnceLock::new();
/// Serializes arm/disarm/finalize transitions.
static CONTROL: Mutex<()> = Mutex::new(());
/// Saved signal state, restored on disarm. Only touched under `CONTROL`.
static SAVED: Mutex<(Option<libc::sigaction>, Option<libc::itimerval>)> = Mutex::new((None, None));
/// The aggregator thread while armed.
static AGGREGATOR: Mutex<Option<Aggregator>> = Mutex::new(None);

struct Aggregator {
    stop: std::sync::Arc<AtomicBool>,
    handle: Option<std::thread::JoinHandle<()>>,
}

static FOLD: std::sync::LazyLock<Mutex<HashMap<Vec<String>, u64>>> =
    std::sync::LazyLock::new(|| Mutex::new(HashMap::new()));

fn out_path() -> PathBuf {
    match crate::utils::flags::profile_out() {
        Some(path) => path.to_path_buf(),
        None => PathBuf::from(format!("capy-profile-{}.folded", std::process::id())),
    }
}

/// Arm the profiler: install the handler, start the 100Hz timer,
/// allocate rings for live mutators, and spawn the aggregator.
/// Idempotent, returns the output path.
pub fn arm() -> Result<PathBuf, String> {
    let _guard = CONTROL.lock().unwrap();
    if ARMED.load(Relaxed) {
        return Ok(OUT_PATH.get().cloned().unwrap_or_else(out_path));
    }
    if !capture::platform_supported() {
        return Err(String::from(
            "SIGPROF profiling is not supported on this platform",
        ));
    }
    let path = OUT_PATH.get_or_init(out_path).clone();
    unsafe {
        let mut action: libc::sigaction = std::mem::zeroed();
        action.sa_sigaction = capture::sigprof_handler as *const () as usize;
        action.sa_flags = libc::SA_SIGINFO | libc::SA_RESTART;
        libc::sigemptyset(&mut action.sa_mask);
        libc::sigaddset(&mut action.sa_mask, libc::SIGPROF);
        let mut saved: libc::sigaction = std::mem::zeroed();
        if libc::sigaction(libc::SIGPROF, &action, &mut saved) != 0 {
            return Err(String::from("sigaction(SIGPROF) failed"));
        }
        // SAFETY: installing our handler while saving the old one, the
        // handler itself only runs on armed mutator threads.
        let interval = libc::timeval {
            tv_sec: 0,
            tv_usec: (1_000_000 / PROFILE_HZ) as libc::suseconds_t,
        };
        let timer = libc::itimerval {
            it_interval: interval,
            it_value: interval,
        };
        let mut saved_timer: libc::itimerval = std::mem::zeroed();
        if libc::setitimer(
            libc::ITIMER_PROF,
            &timer,
            &mut saved_timer as *mut libc::itimerval,
        ) != 0
        {
            libc::sigaction(libc::SIGPROF, &saved, std::ptr::null_mut());
            return Err(String::from("setitimer(ITIMER_PROF) failed"));
        }
        *SAVED.lock().unwrap() = (Some(saved), Some(saved_timer));
    }
    // Armed before enumerating: threads binding concurrently either
    // join the enumeration or allocate at bind
    ARMED.store(true, Relaxed);
    for thread in live_mutators() {
        ensure_ring(&thread);
    }
    let stop = std::sync::Arc::new(AtomicBool::new(false));
    let worker_stop = stop.clone();
    let handle = std::thread::Builder::new()
        .name(String::from("capy-profile-agg"))
        .spawn(move || aggregator_loop(worker_stop))
        .map_err(|err| format!("profiler thread spawn failed: {err}"))?;
    *AGGREGATOR.lock().unwrap() = Some(Aggregator {
        stop,
        handle: Some(handle),
    });
    static REGISTER_ATEXIT: std::sync::Once = std::sync::Once::new();
    REGISTER_ATEXIT.call_once(|| unsafe {
        libc::atexit(finalize_atexit);
    });
    Ok(path)
}

/// Disarm: stop the timer, join the aggregator, and write the final
/// dump.
pub fn disarm() -> PathBuf {
    finalize_inner();
    OUT_PATH.get().cloned().unwrap_or_else(out_path)
}

extern "C" fn finalize_atexit() {
    finalize_inner();
}

fn finalize_inner() {
    let _guard = CONTROL.lock().unwrap();
    if !ARMED.load(Relaxed) {
        return;
    }
    ARMED.store(false, Relaxed);
    unsafe {
        let stop: libc::itimerval = std::mem::zeroed();
        libc::setitimer(
            libc::ITIMER_PROF,
            &stop,
            std::ptr::null_mut() as *mut libc::itimerval,
        );
        // SAFETY: restoring the pre-arm signal state under `CONTROL`.
        if let Some(saved) = SAVED.lock().unwrap().0.take() {
            libc::sigaction(libc::SIGPROF, &saved, std::ptr::null_mut());
        }
    }
    if let Some(mut aggregator) = AGGREGATOR.lock().unwrap().take() {
        aggregator.stop.store(true, Relaxed);
        if let Some(handle) = aggregator.handle.take() {
            let _ = handle.join();
        }
    }
    drain_all();
    write_output();
}

/// Live mutator threads, each `Arc` keeping its thread (and ring)
/// alive past unbind.
fn live_mutators() -> Vec<std::sync::Arc<Thread>> {
    GarbageCollector::get()
        .threads
        .snapshot_threads()
        .into_iter()
        .filter(|thread| thread.is_mutator_thread())
        .collect()
}

/// Allocate `thread`'s ring if
fn ensure_ring(thread: &Thread) {
    use std::sync::atomic::Ordering;
    if thread.prof_ring.load(Ordering::Relaxed).is_null() {
        let ring = Box::into_raw(ProfileRing::new());
        if thread
            .prof_ring
            .compare_exchange(
                std::ptr::null_mut(),
                ring,
                Ordering::Release,
                Ordering::Relaxed,
            )
            .is_err()
        {
            // SAFETY: this box lost the race and was never published.
            drop(unsafe { Box::from_raw(ring) });
        }
    }
}

/// Bind hook (from [`Thread::register`]):
/// mutators binding while armed get a ring.
pub(crate) fn note_thread_bound(thread: &Thread) {
    if ARMED.load(Relaxed) && thread.is_mutator_thread() {
        ensure_ring(thread);
    }
}

/// Unbind hook (from [`Thread::unregister`], before the manager
/// remove): fold this thread's remaining samples while its ring is
/// still reachable.
pub(crate) fn note_thread_unbinding(thread: &Thread) {
    use std::sync::atomic::Ordering;
    let ring = thread.prof_ring.load(Ordering::Relaxed);
    if ring.is_null() {
        return;
    }
    // SAFETY: the ring is owned by `thread`, alive through unbind.
    let ring = unsafe { &*ring };
    let _drain = ring.drain_lock.lock();
    let mut samples = Vec::new();
    ring.drain_into(&mut samples);
    fold_samples(&samples);
}

pub(crate) fn maybe_arm_from_env() {
    if ARMED.load(Relaxed) {
        return;
    }
    if crate::utils::flags::profile_out().is_some()
        && let Err(err) = arm()
    {
        log::warn!("profiler failed to arm: {err}");
    }
}

fn aggregator_loop(stop: std::sync::Arc<AtomicBool>) {
    capture::block_sigprof_for_worker();
    let mut drains = 0u64;
    while !stop.load(Relaxed) {
        std::thread::sleep(DRAIN_INTERVAL);
        if stop.load(Relaxed) {
            break;
        }
        drain_all();
        drains += 1;
        if drains % REAP_EVERY_DRAINS == 0 {
            maps::reap_interp();
        }
        write_output();
    }
}

/// Drain every live mutator's ring into the fold.
fn drain_all() {
    let mut samples = Vec::new();
    for thread in live_mutators() {
        use std::sync::atomic::Ordering;
        let ring = thread.prof_ring.load(Ordering::Relaxed);
        if ring.is_null() {
            continue;
        }
        // SAFETY: the snapshot's `Arc` keeps the thread (and ring)
        // alive through this drain.
        let ring = unsafe { &*ring };
        let _drain = ring.drain_lock.lock();
        ring.drain_into(&mut samples);
    }
    fold_samples(&samples);
}

pub(crate) fn stack_labels(sample: &Sample) -> Vec<String> {
    let mut labels = Vec::with_capacity(sample.nframes as usize);
    for (index, frame) in sample.frames[..sample.nframes as usize].iter().enumerate() {
        if index == 0 {
            labels.push(symbolize_running(sample.rip, sample.pcreg, frame.closure));
        } else {
            labels.push(symbolize_caller(frame.ra, frame.resume, frame.closure));
        }
    }

    labels.reverse();
    labels
}

fn fold_samples(samples: &[Sample]) {
    if samples.is_empty() {
        return;
    }
    let mut fold = FOLD.lock().unwrap();
    for sample in samples {
        *fold.entry(stack_labels(sample)).or_insert(0) += 1;
    }
}

fn header_line() -> String {
    use capture::{
        DROPPED_BAD_FP, DROPPED_EMPTY, DROPPED_NOT_IN_SCHEME, DROPPED_REGS, DROPPED_RINGLESS,
        DROPPED_UNBOUND, OVERWRITTEN, SAMPLES, TORN, TRUNCATED,
    };
    let (unbound, not_in_scheme, ringless, regs, bad_fp, empty) = (
        DROPPED_UNBOUND.load(Relaxed),
        DROPPED_NOT_IN_SCHEME.load(Relaxed),
        DROPPED_RINGLESS.load(Relaxed),
        DROPPED_REGS.load(Relaxed),
        DROPPED_BAD_FP.load(Relaxed),
        DROPPED_EMPTY.load(Relaxed),
    );
    format!(
        "totalSamples {} droppedSamples {} droppedUnbound {} droppedNotInScheme {} droppedRingless {} droppedRegs {} droppedBadFp {} droppedEmpty {} truncatedSamples {} overwrittenSamples {} tornSamples {}\n",
        SAMPLES.load(Relaxed),
        unbound + not_in_scheme + ringless + regs + bad_fp + empty,
        unbound,
        not_in_scheme,
        ringless,
        regs,
        bad_fp,
        empty,
        TRUNCATED.load(Relaxed),
        OVERWRITTEN.load(Relaxed),
        TORN.load(Relaxed),
    )
}

fn write_output() {
    let path = OUT_PATH.get().cloned().unwrap_or_else(out_path);
    let fold = FOLD.lock().unwrap();
    let mut lines: Vec<(&Vec<String>, &u64)> = fold.iter().collect();
    lines.sort_by(|left, right| right.1.cmp(left.1).then(left.0.cmp(right.0)));
    let mut text = header_line();
    for (stack, count) in lines {
        text.push_str(&stack.join(";"));
        text.push(' ');
        text.push_str(&count.to_string());
        text.push('\n');
    }
    drop(fold);
    let temp = path.with_extension("folded.tmp");
    if std::fs::write(&temp, text).is_ok() {
        let _ = std::fs::rename(&temp, &path);
    }
}

#[scheme(path = capy)]
pub mod profile_ops {
    use crate::{
        prelude::*,
        runtime::value::{Str, Value},
    };

    use super::{arm, disarm, make_profile_error};

    /// Arm the profiler, writing folded stacks to `CAPY_PROFILE_OUT`
    /// (or a pid-unique default).
    #[scheme(name = "%profile-start")]
    pub fn profile_start() -> Value<'gc> {
        match arm() {
            Ok(path) => Str::new(ctx, path.to_string_lossy(), true).into(),
            Err(err) => make_profile_error(ctx, err),
        }
    }

    /// Disarm the profiler after a final dump.
    #[scheme(name = "%profile-stop")]
    pub fn profile_stop() -> Value<'gc> {
        let path = disarm();
        Str::new(ctx, path.to_string_lossy(), true).into()
    }
}

fn make_profile_error<'gc>(
    ctx: crate::runtime::MutationContext<'gc>,
    message: String,
) -> crate::runtime::value::Value<'gc> {
    crate::runtime::vm::thunks::make_io_error(
        ctx,
        "%profile-start",
        crate::runtime::value::Str::new(ctx, message, true).into(),
        &[],
    )
}

/// Register the profiler primitives.
pub(crate) fn init_profile<'gc>(ctx: MutationContext<'gc>) {
    profile_ops::register(ctx);
}
