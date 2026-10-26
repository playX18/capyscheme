//! VirtualThreads: Capy threads, scheduled M:N onto worker OS threads.
//!
//! A thread owns a Scheme stack and a Rust stack; Rust and Scheme code run
//! interleaved, and Rust code can equally park a thread and yield to the
//! scheduler. Threads run truly concurrently across the worker pool (one
//! worker per core by default) under preemptive scheduling: a thread that
//! uses its whole quantum is preempted, and a thread that blocks parks and
//! gives its worker back. They are not merely cooperatively scheduled.
//!
//! Spawn with [`spawn`] or [`Builder`], then wait with [`JoinHandle::join`] from
//! a thread or [`JoinHandle::join_blocking`] from a plain OS thread. A thread
//! can be cancelled with [`JoinHandle::kill`]. A killed thread unwinds: its
//! `Drop` impls and Scheme `dynamic-wind` after-thunks run, and its joiners see
//! [`JoinError::Killed`]. Every blocking [`SchedCtx`] call returns
//! `Err(`[`Killed`]`)` on kill.

#![warn(missing_docs)]

#[cfg(not(all(unix, any(target_arch = "x86_64", target_arch = "aarch64"))))]
compile_error!("capy::runtime::sched supports only Unix on x86_64 and AArch64");

#[allow(dead_code)]
pub(crate) mod blocking;
#[allow(dead_code)]
pub(crate) mod context;
#[allow(dead_code)]
pub(crate) mod preempt;
#[allow(dead_code)]
pub(crate) mod queue;
#[allow(dead_code)]
pub(crate) mod reactor;
pub(crate) mod scheduler;
#[allow(dead_code)]
pub(crate) mod stack;
#[allow(dead_code)]
pub(crate) mod sync;
#[allow(dead_code)]
pub(crate) mod sysmon;

mod cabi;

pub use preempt::preemptible;
pub use scheduler::{
    Builder, Config, FdWait, Interest, JoinError, JoinHandle, Killed, Runtime, SchedCtx,
    VirtualThreadId, Waker, spawn,
};
