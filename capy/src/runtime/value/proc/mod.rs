//! Procedure, closure, and executable-code value types.
//!
//! This module contains the runtime representation for callable values. Native
//! procedures are C ABI entrypoints wrapped as heap values, closures pair code
//! with captured values and metadata, and code blocks own or reference the
//! executable bytes used by compiled Scheme code.

use std::{
    cell::{Cell, UnsafeCell},
    collections::{HashMap, VecDeque},
    io,
    mem::MaybeUninit,
    ops::Index,
    sync::{Arc, LazyLock, Mutex as StdMutex},
};

use asmkit::core::jit_allocator::Span;
use mmtk::{AllocationSemantics, util::ObjectReference};

use crate::rsgc::object::{
    AllocationHooks, ClassId, builtin_class_ids, class_header_word,
    class_header_word_with_primitive_layout_tag, primitive_layout_tags,
};
use crate::runtime::{
    Context,
    code_memory::CodeSpan,
    vm::trampolines::{get_cont_trampoline_from_scheme, get_trampoline_from_scheme},
};
use crate::{
    IndexWrite, WeakProcessor,
    rsgc::{
        Global, cell::Lock, collection::Visitor, finalizer::FinalizerQueue, sync::monitor::Monitor,
    },
};

use super::*;

mod closure;
mod code;
mod native;
mod registry;
mod relocatable;

pub use closure::*;
pub use code::*;
pub use native::*;
pub use registry::*;
pub use relocatable::*;

#[cfg(test)]
mod tests;
