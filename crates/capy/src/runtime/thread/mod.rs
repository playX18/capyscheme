//! Thread and GC context management.

use crate::{
    heap::{GarbageCollector, Gc, Mutation, Mutator, Trace, sync::thread::Thread},
    prelude::{HashTable, Keyword},
    runtime::stats::ThreadStats,
};
use crate::{
    prelude::{IntoValue, NativeFn, PROCEDURES},
    runtime::{
        fluids::DynamicState,
        global::{VM_STATE, VmState},
        interp::stack::{Ip, Slot, StackState},
        //image::{ALLOWED_GC, AllowedGc, reader::ImageReader},
        linklet::{
            LibraryInstance, Linklet, instantiate_linklet, registry_register_new, root_registry,
        },
        objectmodel::{
            Closure, ReturnCode, Str, Symbol, Value, init_symbols, init_weak_sets, init_weak_tables,
        },
        prelude::VariableRef,
        root::trace_root_guarded,
        vm::{
            ExecutionResult, base::program_arguments_fluid, control::ContinuationMarks,
            load::load_thunk_in_vicinity, threading::ThreadObject,
        },
    },
};
use std::{
    cell::Cell,
    sync::{
        Once,
        atomic::{AtomicPtr, AtomicUsize},
    },
};

/// Historical name of [`Mutation`], kept so call sites need not change.
mod context;
mod engine;
mod mutation;
mod scheme;
mod state;

pub use context::ThreadContext;
pub use engine::Engine;
pub use mutation::MutationContext;
pub(crate) use scheme::SCM_INITIALIZED;
pub use scheme::Scheme;
pub use state::SchemeState;
