//! Procedure, closure, and executable-code value types.
//!
//! This module contains the runtime representation for callable values. Native
//! procedures are C ABI entrypoints wrapped as heap values, closures pair code
//! with captured values and metadata, and code blocks own or reference the
//! executable bytes used by compiled Scheme code.

use std::{collections::HashMap, ops::Index, sync::LazyLock};

use crate::heap::object::{
    AllocationHooks, ClassId, builtin_class_ids, class_header_word,
    class_header_word_with_private_variant_flag,
};
use crate::runtime::MutationContext;
use crate::{
    IndexWrite, WeakProcessor,
    heap::{Global, cell::Lock, collection::Visitor, sync::monitor::Monitor},
};

use super::*;

mod closure;
mod code;
mod native;
mod registry;

pub use closure::*;
pub use code::*;
pub use native::*;
pub use registry::*;
