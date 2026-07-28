//! Core marking constraints registered at GC init.

use super::{
    ConstraintParallelism, ConstraintVolatility, MarkingConstraintSet, SimpleMarkingConstraint,
};
use crate::heap::GarbageCollector;

/// Register built-in constraints (OopStorage, globals, weak/ephemeron, finalizers).
pub fn add_core_constraints(set: &mut MarkingConstraintSet) {
    // Strong OopStorage — also scanned in scan_vm_specific_roots; constraint keeps the
    // GreyedByExecution slot for wavefront accounting / re-entry after mutator resume.
    set.add(Box::new(SimpleMarkingConstraint::new(
        "Os",
        "OopStorage Strong",
        ConstraintVolatility::GreyedByExecution,
        |_visitor| {
            // Roots are enqueued via RootsWorkFactory in scan_vm_specific_roots.
            // This constraint is a no-op body; presence drives scheduling heuristics.
        },
    )));

    set.add(Box::new(SimpleMarkingConstraint::new(
        "Gl",
        "Global Registry",
        ConstraintVolatility::GreyedByExecution,
        |_visitor| {},
    )));

    // Weak / ephemeron outgrowths — body is a marker; real work is in WeakProcessingState.
    set.add(Box::new(
        SimpleMarkingConstraint::new(
            "Wk",
            "Weak Refs",
            ConstraintVolatility::GreyedByMarking,
            |_visitor| {},
        )
        .with_parallelism(ConstraintParallelism::Parallel),
    ));

    set.add(Box::new(SimpleMarkingConstraint::new(
        "Fn",
        "Finalizers",
        ConstraintVolatility::GreyedByMarking,
        |_visitor| {},
    )));
}

/// Add a custom constraint. Panics if a collection is in progress.
pub fn add_marking_constraint(constraint: Box<dyn super::MarkingConstraint>) {
    let gc = GarbageCollector::get();
    let mut set = gc.constraints.write();
    set.add(constraint);
}
