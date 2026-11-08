//! Linklet runtime objects.

use std::{cell::Cell, sync::OnceLock};

use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Gc, Trace, barrier, cell::Lock};

use crate::runtime::{
    MutationContext,
    objectmodel::{ClassTagged, HashTable, HashTableType, IntoValue, Symbol, Value},
    variable::Variable,
};


mod instance;
mod linklet;
mod registry;

pub use instance::LibraryInstance;
pub use linklet::Linklet;
pub use registry::*;
