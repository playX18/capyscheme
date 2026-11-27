//! Thread introspection: what every live thread is doing, what it is waiting
//! for, and which threads are deadlocked.
//!
//! A snapshot is cheap and never stops the world. Each task keeps its wait
//! record in a few atomics (see [`wait`]), written by the thread itself just
//! before it parks and cleared when it resumes. Who holds a mutex, and which
//! thread a joiner waits for, is read from the wait queues in `sync.rs`.
//! Nothing here touches the Scheme heap, so it is safe to call from any
//! OS thread, including a signal-driven dumper.

use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::time::Duration;

use super::scheduler;
use super::sync::{self, WaitEdge};

/// What a parked thread waits for. Stored in `Task::wait_kind`.
pub(crate) mod wait {
    /// Not waiting.
    pub(crate) const NONE: u8 = 0;
    /// Waiting to acquire a mutex. The argument is the mutex address.
    pub(crate) const MUTEX: u8 = 1;
    /// Waiting on a condition variable. The argument is the condition address.
    pub(crate) const CONDITION: u8 = 2;
    /// Waiting for another thread to finish. The argument is its id.
    pub(crate) const JOIN: u8 = 3;
    /// Sleeping. The argument is the requested duration in nanoseconds
    /// (zero when the deadline belongs to some other wait).
    pub(crate) const SLEEP: u8 = 4;
    /// Waiting for a file descriptor. The argument is the fd.
    pub(crate) const IO: u8 = 5;
    /// Parked without a more specific reason.
    pub(crate) const PARK: u8 = 6;

    /// The name Scheme and dumps use for a wait kind.
    pub(crate) fn name(kind: u8) -> &'static str {
        match kind {
            MUTEX => "mutex",
            CONDITION => "condition",
            JOIN => "join",
            SLEEP => "sleep",
            IO => "io",
            PARK => "park",
            _ => "none",
        }
    }
}

/// Where a thread is in its life.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum State {
    /// Queued, or about to be.
    Runnable,
    /// On a worker.
    Running,
    /// Parked.
    Blocked,
    /// Finished, but not yet forgotten.
    Done,
}

impl State {
    pub(crate) fn name(self) -> &'static str {
        match self {
            State::Runnable => "runnable",
            State::Running => "running",
            State::Blocked => "blocked",
            State::Done => "done",
        }
    }
}

/// One thread, as seen by a snapshot.
#[derive(Clone, Debug)]
pub(crate) struct ThreadInfo {
    pub(crate) id: u64,
    /// The thread that spawned this one, or zero.
    pub(crate) parent: u64,
    pub(crate) name: Option<String>,
    pub(crate) state: State,
    /// Current queue level and base priority.
    pub(crate) level: u8,
    pub(crate) base: u8,
    /// The worker it last ran on.
    pub(crate) worker: usize,
    /// The worker it is restricted to, if any.
    pub(crate) pinned: Option<usize>,
    pub(crate) wait_kind: u8,
    pub(crate) wait_arg: u64,
    /// How long the current wait has lasted, zero when not waiting.
    pub(crate) waiting: Duration,
    /// Time since the thread was spawned.
    pub(crate) age: Duration,
    /// A kill is pending or in progress.
    pub(crate) killed: bool,
}

/// Every thread that has not finished, ordered by id.
pub(crate) fn threads() -> Vec<ThreadInfo> {
    scheduler::snapshot()
}

/// Edges "thread A waits for thread B" for every blocked mutex acquire and
/// join. Condition waits have no edge: any thread may signal them.
pub(crate) fn edges() -> Vec<WaitEdge> {
    sync::wait_edges(crate::runtime::vm::threading::mutex_owner_at)
}

/// Cycles in the waits-for graph. Each thread waits for at most one thing, so
/// the graph has out-degree one and a walk finds every cycle exactly once.
/// A cycle starts at its smallest id, and cycles are ordered by that id.
pub(crate) fn find_cycles(edges: &[WaitEdge]) -> Vec<Vec<u64>> {
    let next: HashMap<u64, u64> = edges
        .iter()
        .filter_map(|e| Some((e.waiter, e.holder?)))
        .collect();
    let mut starts: Vec<u64> = next.keys().copied().collect();
    starts.sort_unstable();

    let mut done: HashSet<u64> = HashSet::new();
    let mut cycles = Vec::new();
    for start in starts {
        if done.contains(&start) {
            continue;
        }
        let mut path: Vec<u64> = Vec::new();
        let mut position: HashMap<u64, usize> = HashMap::new();
        let mut cur = start;
        loop {
            if let Some(&i) = position.get(&cur) {
                let mut cycle = path[i..].to_vec();
                let smallest = cycle
                    .iter()
                    .enumerate()
                    .min_by_key(|(_, id)| **id)
                    .map_or(0, |(i, _)| i);
                cycle.rotate_left(smallest);
                cycles.push(cycle);
                break;
            }
            if done.contains(&cur) {
                break;
            }
            position.insert(cur, path.len());
            path.push(cur);
            match next.get(&cur) {
                Some(&n) => cur = n,
                None => break,
            }
        }
        done.extend(path);
    }
    cycles.sort();
    cycles
}

fn thread_label(id: u64, by_id: &HashMap<u64, &ThreadInfo>) -> String {
    if id & (1 << 63) != 0 {
        return "an OS thread".into();
    }
    match by_id.get(&id).and_then(|t| t.name.as_deref()) {
        Some(name) => format!("#{id} ({name})"),
        None => format!("#{id}"),
    }
}

fn fmt_duration(d: Duration) -> String {
    let ms = d.as_millis();
    if ms < 1000 {
        format!("{ms}ms")
    } else {
        format!("{:.2}s", d.as_secs_f64())
    }
}

/// A text report of every thread, in the spirit of a JVM thread dump:
/// one line per thread, who waits for whom, and any deadlock found.
pub(crate) fn dump() -> String {
    let threads = threads();
    let edges = edges();
    let by_id: HashMap<u64, &ThreadInfo> = threads.iter().map(|t| (t.id, t)).collect();
    let holder_of: HashMap<u64, &WaitEdge> = edges.iter().map(|e| (e.waiter, e)).collect();

    let blocked = threads.iter().filter(|t| t.state == State::Blocked).count();
    let mut out = String::new();
    let _ = writeln!(out, "{} threads, {blocked} blocked", threads.len());
    for t in &threads {
        let _ = write!(out, "  #{:<4} {:<9}", t.id, t.state.name());
        if let Some(name) = &t.name {
            let _ = write!(out, " {name}");
        }
        if t.state == State::Blocked && t.wait_kind != wait::NONE {
            let _ = write!(out, " on {}", wait::name(t.wait_kind));
            match t.wait_kind {
                wait::MUTEX | wait::CONDITION => {
                    let _ = write!(out, " {:#x}", t.wait_arg);
                }
                wait::JOIN => {
                    let _ = write!(out, " {}", thread_label(t.wait_arg, &by_id));
                }
                wait::IO => {
                    let _ = write!(out, " fd {}", t.wait_arg);
                }
                _ => {}
            }
            if let Some(holder) = holder_of.get(&t.id).and_then(|e| e.holder)
                && t.wait_kind == wait::MUTEX
            {
                let _ = write!(out, ", held by {}", thread_label(holder, &by_id));
            }
            let _ = write!(out, ", for {}", fmt_duration(t.waiting));
        }
        let _ = write!(out, "  [worker {}", t.worker);
        if let Some(w) = t.pinned {
            let _ = write!(out, " pinned {w}");
        }
        let _ = write!(out, ", level {}", t.level);
        if t.parent != 0 {
            let _ = write!(out, ", parent #{}", t.parent);
        }
        let _ = write!(out, ", age {}", fmt_duration(t.age));
        if t.killed {
            let _ = write!(out, ", killed");
        }
        let _ = writeln!(out, "]");
    }
    for cycle in find_cycles(&edges) {
        let mut chain: Vec<String> = cycle.iter().map(|&id| thread_label(id, &by_id)).collect();
        chain.push(thread_label(cycle[0], &by_id));
        let _ = writeln!(
            out,
            "deadlock among {} thread{}: {}",
            cycle.len(),
            if cycle.len() == 1 { "" } else { "s" },
            chain.join(" -> ")
        );
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn edge(waiter: u64, holder: Option<u64>) -> WaitEdge {
        WaitEdge {
            waiter,
            kind: wait::MUTEX,
            holder,
        }
    }

    #[test]
    fn no_edges_no_cycles() {
        assert!(find_cycles(&[]).is_empty());
    }

    #[test]
    fn a_chain_is_not_a_cycle() {
        let edges = [edge(1, Some(2)), edge(2, Some(3)), edge(3, None)];
        assert!(find_cycles(&edges).is_empty());
    }

    #[test]
    fn a_two_cycle_starts_at_its_smallest_id() {
        let edges = [edge(9, Some(4)), edge(4, Some(9))];
        assert_eq!(find_cycles(&edges), vec![vec![4, 9]]);
    }

    #[test]
    fn a_thread_waiting_for_itself_is_a_cycle() {
        assert_eq!(find_cycles(&[edge(5, Some(5))]), vec![vec![5]]);
    }

    #[test]
    fn a_tail_leading_into_a_cycle_reports_only_the_cycle() {
        let edges = [
            edge(1, Some(2)),
            edge(2, Some(3)),
            edge(3, Some(4)),
            edge(4, Some(2)),
        ];
        assert_eq!(find_cycles(&edges), vec![vec![2, 3, 4]]);
    }

    #[test]
    fn two_separate_cycles_are_both_found_once() {
        let edges = [
            edge(1, Some(2)),
            edge(2, Some(1)),
            edge(7, Some(8)),
            edge(8, Some(9)),
            edge(9, Some(7)),
        ];
        assert_eq!(find_cycles(&edges), vec![vec![1, 2], vec![7, 8, 9]]);
    }
}
