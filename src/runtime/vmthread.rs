//! Finalizer thread

use std::sync::{
    Arc, LazyLock,
    atomic::{AtomicBool, Ordering},
    mpsc,
};

use rsgc::{Mutator, Rootable, context::Mutation, vmkit::sync::Monitor};

pub enum VMThreadTask {
    /// Vacuum weak sets. This task will walk all live weak sets and remove
    /// any broken weak entries in them. This task is ran in mutator context
    /// thus it won't run until GC is complete.
    VacuumWeakSets,
    VacuumWeakTables,
    MutatorTask(Box<dyn for<'gc> FnOnce(&'gc Mutation<'gc>) + Send>),
    Task(Box<dyn FnOnce() + Send>),

    /// Shutdown the VM thread. This task will cause the thread to exit.
    Shutdown,
}

pub static VM_THREAD: LazyLock<VmThread> = LazyLock::new(|| VmThread::new());

/// VM thread that handles execution of various tasks.
///
/// Mainly used for finalization tasks that are not time-critical and can be processed in the background.
pub struct VmThread {
    sender: mpsc::Sender<VMThreadTask>,
    thread_handle: Option<std::thread::JoinHandle<()>>,
    pair: Arc<Monitor<AtomicBool>>,
}

impl VmThread {
    pub fn new() -> Self {
        let (sender, receiver) = mpsc::channel::<VMThreadTask>();
        let pair = Arc::new(Monitor::new(AtomicBool::new(false)));
        let thread_pair = Arc::clone(&pair);

        let thread_handle = std::thread::spawn(move || {
            let mutator = Mutator::<Rootable!(())>::new(|_| ());
            loop {
                // Wait until there's work or a shutdown signal.
                let mut guard = thread_pair.lock_no_handshake();
                while !guard.load(Ordering::Relaxed) {
                    guard.wait_no_handshake();
                }
                guard.store(false, Ordering::Relaxed); // Reset the flag
                // Process all available tasks
                let mut shutdown_requested = false;
                while let Ok(task) = receiver.try_recv() {
                    match task {
                        VMThreadTask::VacuumWeakSets => {
                            mutator.mutate(|mc, _| {
                                super::value::weak_set::vacuum_weak_sets(mc);
                            });
                        }
                        VMThreadTask::VacuumWeakTables => {
                            mutator.mutate(|mc, _| {
                                super::value::weak_table::vacuum_weak_tables(mc);
                            });
                        }
                        
                        VMThreadTask::MutatorTask(task) => {
                            mutator.mutate(|mc, _| {
                                task(mc);
                            });
                        }

                        VMThreadTask::Task(task) => {
                            task();
                        }

                        VMThreadTask::Shutdown => {
                            shutdown_requested = true;
                            break; // Exit task processing loop
                        }
                    }
                }

                if shutdown_requested {
                    break; // Exit thread loop
                }
            }
        });

        VmThread {
            sender,
            thread_handle: Some(thread_handle),
            pair,
        }
    }

    /// Sends a task to the VM thread and notifies it.
    pub fn schedule_task(&self, task: VMThreadTask) {
        if self.sender.send(task).is_ok() {
            let guard = self.pair.lock_no_handshake();
            guard.store(true, Ordering::Relaxed);
            guard.notify();
        } else {
            // This might happen if the receiver (VM thread) has already panicked or exited.
            panic!("VMThread: Failed to send task. Thread might be down.");
        }
    }

    /// Signals the VM thread to shut down and waits for it to complete.
    pub fn shutdown(mut self) {
        if self.sender.send(VMThreadTask::Shutdown).is_ok() {
            let guard = self.pair.lock_no_handshake();
            guard.store(true, Ordering::Relaxed);
            guard.notify();
        }

        if let Some(handle) = self.thread_handle.take() {
            handle.join().expect("VM thread panicked");
        }
    }
}

impl Drop for VmThread {
    fn drop(&mut self) {
        if self.thread_handle.is_some() {
            if self.sender.send(VMThreadTask::Shutdown).is_ok() {
                let guard = self.pair.lock_no_handshake();
                guard.store(true, Ordering::Relaxed);
                guard.notify();
            }
        }
    }
}
