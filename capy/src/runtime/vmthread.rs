use std::{
    mem::transmute,
    sync::{
        Arc, LazyLock,
        atomic::{AtomicBool, Ordering},
        mpsc::{self, RecvTimeoutError},
    },
    time::Duration,
};

use crate::rsgc::{GarbageCollector, Mutation, Mutator, mmtk, sync::monitor::Monitor};

pub enum Command {
    /// Vacuum weak sets. This task walks all live weak sets and removes broken
    /// entries. It runs in mutator context, so it waits until GC completes.
    VacuumWeakSets,
    VacuumWeakTables,
    ClosePorts,
    FinalizePointers,
    MutatorTask(Box<dyn for<'gc> FnOnce(&'gc Mutation<'gc>) + Send>),
    Task(Box<dyn FnOnce() + Send>),

    /// Shutdown the VM thread. This task will cause the thread to exit.
    Shutdown,
}

pub static VM_THREAD: LazyLock<TaskThread> = LazyLock::new(TaskThread::new);

/// Background thread for VM maintenance and finalization tasks.
pub struct TaskThread {
    sender: mpsc::Sender<Command>,
    thread_handle: Option<std::thread::JoinHandle<()>>,
    pair: Arc<Monitor<AtomicBool>>,
}

impl TaskThread {
    pub fn new() -> Self {
        let (sender, receiver) = mpsc::channel::<Command>();
        let pair = Arc::new(Monitor::new(AtomicBool::new(false)));
        let thread_pair = Arc::clone(&pair);

        let thread_handle = std::thread::spawn(move || {
            let mutator = Mutator::<crate::Rootable!(())>::new(|_| ());
            loop {
                // Wait until there's work or a shutdown signal.
                let mut guard = thread_pair.lock();
                while !guard.load(Ordering::Relaxed) {
                    guard.wait_for(Duration::from_millis(100));
                }
                guard.store(false, Ordering::Relaxed); // Reset the flag
                drop(guard);

                // Process all available tasks
                let mut shutdown_requested = false;
                loop {
                    match receiver.recv_timeout(Duration::from_millis(100)) {
                        Ok(task) => match task {
                            Command::FinalizePointers => (),
                            Command::ClosePorts => {
                                // TODO(Adel): implement port closing on thread shutdown
                            }
                            Command::VacuumWeakSets => {
                                mutator.mutate(|mc, _| {
                                    super::value::weak_set::vacuum_weak_sets(mc);
                                });
                            }
                            Command::VacuumWeakTables => {
                                mutator.mutate(|mc, _| {
                                    super::value::weak_table::vacuum_weak_tables(mc);
                                });
                            }

                            Command::MutatorTask(task) => {
                                mutator.mutate(|mc, _| {
                                    task(&mc);
                                });
                            }

                            Command::Task(task) => {
                                task();
                            }

                            Command::Shutdown => {
                                shutdown_requested = true;
                                break; // Exit task processing loop
                            }
                        },
                        Err(RecvTimeoutError::Timeout) => {
                            mutator.mutate(|mc, _| {
                                mmtk::memory_manager::gc_poll(
                                    &GarbageCollector::get().mmtk,
                                    // SAFETY: Preconditions verified by the surrounding code
                                    unsafe {
                                        // SAFETY: `mc.thread()` and `VMMutatorThread` are both
                                        // thin wrappers around the same `*mut Thread` pointer.
                                        // The RSGC crate guarantees layout compatibility.
                                        transmute::<
                                            &crate::rsgc::sync::thread::Thread,
                                            crate::rsgc::mmtk::util::VMMutatorThread,
                                        >(mc.thread())
                                    },
                                );
                            });
                        }

                        Err(_) => break,
                    }
                }

                if shutdown_requested {
                    break; // Exit thread loop
                }
            }
        });

        Self {
            sender,
            thread_handle: Some(thread_handle),
            pair,
        }
    }

    /// Sends a task to the VM thread and notifies it.
    pub fn schedule_task(&self, task: Command) {
        if self.sender.send(task).is_ok() {
            let guard = self.pair.lock();
            guard.store(true, Ordering::Relaxed);
            guard.notify_all();
        } else {
            // This might happen if the receiver (VM thread) has already panicked or exited.
            panic!("VM task thread is unavailable");
        }
    }

    /// Signals the VM thread to shut down and waits for it to complete.
    pub fn shutdown(mut self) {
        if self.sender.send(Command::Shutdown).is_ok() {
            let guard = self.pair.lock();
            guard.store(true, Ordering::Relaxed);
            guard.notify_all();
        }

        if let Some(handle) = self.thread_handle.take() {
            handle.join().expect("VM thread panicked");
        }
    }
}

impl Default for TaskThread {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for TaskThread {
    fn drop(&mut self) {
        if self.thread_handle.is_some() && self.sender.send(Command::Shutdown).is_ok() {
            let guard = self.pair.lock();
            guard.store(true, Ordering::Relaxed);
            guard.notify_all();
        }
    }
}
