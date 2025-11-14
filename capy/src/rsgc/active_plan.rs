use std::sync::Arc;

use parking_lot::MutexGuard;

use crate::rsgc::{mm::MemoryManager, sync::thread::Thread};

#[derive(Default)]
pub struct ActivePlan;

pub struct MutatorIterator<'a> {
    guard: MutexGuard<'a, Vec<Arc<Thread>>>,
    index: usize,
}

impl<'a> Iterator for MutatorIterator<'a> {
    type Item = &'a mut mmtk::Mutator<MemoryManager>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.index < self.guard.len() {
            let thread = &self.guard[self.index];
            self.index += 1;
            if thread.is_mutator() {
                return Some(unsafe { thread.mutator_unchecked() });
            }
        }
        None
    }
}
impl mmtk::vm::ActivePlan<MemoryManager> for ActivePlan {
    fn is_mutator(tls: mmtk::util::VMThread) -> bool {
        let thread = Thread::from_vmthread(tls);
        thread.is_mutator()
    }

    fn mutator(tls: mmtk::util::VMMutatorThread) -> &'static mut mmtk::Mutator<MemoryManager> {
        unsafe { Thread::from_mutator_thread(tls).mutator_unchecked() }
    }

    fn mutators<'a>() -> Box<dyn Iterator<Item = &'a mut mmtk::Mutator<MemoryManager>> + 'a> {
        Box::new(MutatorIterator {
            guard: super::GarbageCollector::get().threads.threads(),
            index: 0,
        })
    }

    fn number_of_mutators() -> usize {
        Self::mutators().count()
    }
}
