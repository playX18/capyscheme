//! The sysmon thread: enforces the thread time quantum and runs the
//! priority boost.
//!
//! Every [`TICK`] it looks at each worker's current slice. A slice seen
//! running for a whole quantum (counted in ticks, so the quantum is rounded up
//! to a tick), while some other thread is queued, gets its preempt flag set
//! and a yieldpoint requested (see `Thread::preempt_running`). A slice is also
//! preempted early when a thread more urgent than it is queued on the
//! global queue or on its worker, and no worker is free to take that one.
//! Every [`BOOST_TICKS`] all queued threads return to their base level.

use std::sync::Arc;
use std::sync::atomic::Ordering;
use std::time::Duration;

use super::queue::Shared;

/// Sampling period. A slice can overrun its quantum by at most this much.
pub(crate) const TICK: Duration = Duration::from_millis(2);

/// Ticks between priority boosts (100 ms).
const BOOST_TICKS: u64 = 50;

pub(crate) fn spawn(shared: Arc<Shared>) {
    std::thread::Builder::new()
        .name("capy-sysmon".into())
        .spawn(move || sysmon_main(shared))
        .expect("failed to start sysmon thread");
}

fn sysmon_main(shared: Arc<Shared>) {
    // Ticks a slice has been seen running, per worker, keyed by its epoch.
    let mut seen = vec![(0u64, 0u64); shared.slots.len()];
    let quantum_ticks = shared.quantum.as_nanos().div_ceil(TICK.as_nanos()) as u64;
    let mut ticks = 0u64;
    loop {
        std::thread::sleep(TICK);
        if shared.stop.load(Ordering::Relaxed) {
            return;
        }
        ticks += 1;
        if ticks % BOOST_TICKS == 0 {
            shared.boost();
        }
        for (index, (slot, seen)) in shared.slots.iter().zip(seen.iter_mut()).enumerate() {
            let id = slot.slice_id.load(Ordering::Acquire);
            if id != seen.0 {
                *seen = (id, 0);
                continue;
            }
            if id == 0 {
                continue;
            }
            seen.1 += 1;

            // Nothing waiting means preemption would only add overhead.
            if shared.queued() == 0 {
                continue;
            }

            // A more urgent thread waiting is worth a preemption now, but
            // only when no worker is free to take it.
            let running = slot.running_level.load(Ordering::SeqCst);
            let urgent = !shared.worker_free() && shared.outranked(index, running);
            if seen.1 < quantum_ticks && !urgent {
                continue;
            }
            let Some(thread) = slot.thread.get() else {
                continue;
            };
            thread.preempt_running(|g| !g.sched.task_is_null() && g.sched.epoch.get() == id);
        }
    }
}
