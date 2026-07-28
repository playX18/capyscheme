//! Marking constraint registry and wavefront convergence (JSC MarkingConstraintSet).

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use mmtk::{
    scheduler::GCWorker,
    vm::{ObjectTracer, ObjectTracerContext},
};

use super::{
    ConstraintParallelism, ConstraintVisitor, ConstraintVolatility, MarkingConstraint,
    solver::{self, ConstraintExecuteMode},
};
use crate::heap::mm::MemoryManager;

pub struct MarkingConstraintSet {
    constraints: Vec<Box<dyn MarkingConstraint>>,
    /// Indices of GreyedByExecution constraints.
    roots: Vec<usize>,
    /// Indices of GreyedByMarking constraints.
    outgrowths: Vec<usize>,
    iteration: AtomicUsize,
    /// Set when any constraint greys/retains work in the current pass.
    dirty: AtomicBool,
    /// Armed when parallel VMRefClosure shards are scheduled; consumed on the
    /// post-drain `process_weak_refs` re-entry so we do not reschedule forever.
    parallel_reentry: AtomicBool,
    /// Prevents adding constraints during an active collection.
    collecting: AtomicBool,
}

impl MarkingConstraintSet {
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
            roots: Vec::new(),
            outgrowths: Vec::new(),
            iteration: AtomicUsize::new(1),
            dirty: AtomicBool::new(false),
            parallel_reentry: AtomicBool::new(false),
            collecting: AtomicBool::new(false),
        }
    }

    pub fn add(&mut self, constraint: Box<dyn MarkingConstraint>) {
        assert!(
            !self.collecting.load(Ordering::Acquire),
            "cannot add marking constraints during a collection"
        );
        let index = self.constraints.len();
        match constraint.volatility() {
            ConstraintVolatility::GreyedByExecution => self.roots.push(index),
            ConstraintVolatility::GreyedByMarking => self.outgrowths.push(index),
            ConstraintVolatility::SeldomGreyed => {}
        }
        self.constraints.push(constraint);
    }

    pub fn did_start_marking(&self) {
        self.collecting.store(true, Ordering::Release);
        self.iteration.store(1, Ordering::Relaxed);
        self.dirty.store(false, Ordering::Relaxed);
        self.parallel_reentry.store(false, Ordering::Relaxed);
    }

    pub fn did_finish_marking(&self) {
        self.collecting.store(false, Ordering::Release);
        self.iteration.store(1, Ordering::Relaxed);
        self.parallel_reentry.store(false, Ordering::Relaxed);
    }

    pub fn mark_dirty(&self) {
        self.dirty.store(true, Ordering::Relaxed);
    }

    pub fn take_dirty(&self) -> bool {
        self.dirty.swap(false, Ordering::Relaxed)
    }

    pub(crate) fn arm_parallel_reentry(&self) {
        self.parallel_reentry.store(true, Ordering::Relaxed);
    }

    pub(crate) fn take_parallel_reentry(&self) -> bool {
        self.parallel_reentry.swap(false, Ordering::Relaxed)
    }

    pub fn len(&self) -> usize {
        self.constraints.len()
    }

    pub fn is_empty(&self) -> bool {
        self.constraints.is_empty()
    }

    fn work_estimate(&self, index: usize, visitor: &ConstraintVisitor<'_>) -> f64 {
        self.constraints[index].quick_work_estimate(visitor)
    }

    fn is_wavefront_advancing(&self, visitor: &ConstraintVisitor<'_>) -> bool {
        self.outgrowths
            .iter()
            .any(|&i| self.work_estimate(i, visitor) > 0.0)
    }

    /// Ordered indices for the current convergence iteration (JSC heuristics).
    pub fn ordered_for_iteration(
        &self,
        iteration: usize,
        visitor: &ConstraintVisitor<'_>,
    ) -> Vec<usize> {
        if iteration == 1 {
            return self.roots.clone();
        }
        if iteration == 2 {
            return self.outgrowths.clone();
        }

        let advancing = self.is_wavefront_advancing(visitor);
        let mut ordered: Vec<usize> = (0..self.constraints.len()).collect();
        ordered.sort_by(|&a, &b| {
            let vol = |i: usize| {
                if self.constraints[i].volatility() == ConstraintVolatility::GreyedByMarking {
                    1u8
                } else {
                    0
                }
            };
            let (va, vb) = (vol(a), vol(b));
            if va != vb {
                return if advancing { vb.cmp(&va) } else { va.cmp(&vb) };
            }
            let (wa, wb) = (
                self.work_estimate(a, visitor),
                self.work_estimate(b, visitor),
            );
            if (wa - wb).abs() > f64::EPSILON {
                return wb.partial_cmp(&wa).unwrap_or(std::cmp::Ordering::Equal);
            }
            self.constraints[b]
                .volatility()
                .cmp(&self.constraints[a].volatility())
        });
        ordered
    }

    /// Run GreyedByMarking (and later iterations) during `VMRefClosure`.
    ///
    /// Returns `true` if MMTk should invoke `process_weak_refs` again after draining.
    pub fn execute_vmref_convergence<C: ObjectTracerContext<MemoryManager> + Clone + 'static>(
        &self,
        worker: &mut GCWorker<MemoryManager>,
        tracer_context: &mut C,
    ) -> bool {
        let iteration = self.iteration.fetch_add(1, Ordering::Relaxed);
        // Skip pure-root iteration here — roots already ran in Prepare / mutator scan.
        let iteration = iteration.max(2);
        let counting = ConstraintVisitor::new_counting();
        let order = self.ordered_for_iteration(iteration, &counting);
        // Prefer outgrowths for VMRefClosure; include SeldomGreyed on late iterations.
        let order: Vec<usize> = if iteration == 2 {
            self.outgrowths.clone()
        } else {
            order
                .into_iter()
                .filter(|&i| {
                    matches!(
                        self.constraints[i].volatility(),
                        ConstraintVolatility::GreyedByMarking | ConstraintVolatility::SeldomGreyed
                    )
                })
                .collect()
        };

        if order.is_empty() {
            return self.take_dirty();
        }

        let mode = if solver::PARALLEL_CONSTRAINTS.load(Ordering::Relaxed)
            && order
                .iter()
                .any(|&i| self.constraints[i].parallelism() == ConstraintParallelism::Parallel)
        {
            ConstraintExecuteMode::Parallel
        } else {
            ConstraintExecuteMode::Sequential
        };

        let visited = solver::execute_constraints(self, &order, worker, tracer_context, mode);
        // #region agent log
        {
            use std::io::Write;
            if let Ok(mut f) = std::fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open("/home/adel/projects/capyscheme/.cursor/debug-3ed3b0.log")
            {
                let _ = writeln!(
                    f,
                    r#"{{"sessionId":"3ed3b0","runId":"post-fix","hypothesisId":"A","location":"set.rs:execute_vmref_convergence","message":"vmref_convergence","data":{{"iteration":{},"order_len":{},"visited":{},"dirty":{}}},"timestamp":{}}}"#,
                    iteration,
                    order.len(),
                    visited,
                    self.dirty.load(Ordering::Relaxed),
                    std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .map(|d| d.as_millis())
                        .unwrap_or(0)
                );
            }
        }
        // #endregion
        // Iteration 2 always requests another pass (JSC); later passes repeat while dirty.
        // Do not mark_dirty merely because a pass requested re-entry (e.g. parallel drain).
        if iteration == 2 {
            return true;
        }
        visited || self.take_dirty()
    }

    /// Execute a single constraint by index with the given tracer.
    pub(crate) fn execute_one(&self, index: usize, tracer: &mut dyn ObjectTracer) -> usize {
        let mut visitor = ConstraintVisitor::new_tracing(tracer);
        self.constraints[index].prepare_to_execute();
        self.constraints[index].execute(&mut visitor);
        visitor.visit_count()
    }

    pub(crate) fn constraint(&self, index: usize) -> &dyn MarkingConstraint {
        &*self.constraints[index]
    }

    pub fn outgrowth_indices(&self) -> &[usize] {
        &self.outgrowths
    }

    pub fn root_indices(&self) -> &[usize] {
        &self.roots
    }
}

impl Default for MarkingConstraintSet {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::heap::constraints::{ConstraintVolatility, SimpleMarkingConstraint};

    #[test]
    fn registers_roots_and_outgrowths() {
        let mut set = MarkingConstraintSet::new();
        set.add(Box::new(SimpleMarkingConstraint::new(
            "R",
            "Root",
            ConstraintVolatility::GreyedByExecution,
            |_| {},
        )));
        set.add(Box::new(SimpleMarkingConstraint::new(
            "O",
            "Outgrowth",
            ConstraintVolatility::GreyedByMarking,
            |_| {},
        )));
        assert_eq!(set.roots, vec![0]);
        assert_eq!(set.outgrowths, vec![1]);
        let visitor = ConstraintVisitor::new_counting();
        assert_eq!(set.ordered_for_iteration(1, &visitor), vec![0]);
        assert_eq!(set.ordered_for_iteration(2, &visitor), vec![1]);
    }
}
