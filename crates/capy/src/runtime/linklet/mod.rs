//! Linklet runtime objects.

use std::{cell::Cell, sync::OnceLock};

use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Gc, Trace, barrier, cell::Lock};

use crate::runtime::{
    MutationContext,
    objectmodel::{ClassTagged, HashTable, HashTableType, IntoValue, Symbol, Value, Vector},
    variable::Variable,
};

mod directory;
mod effects;
mod instance;
#[allow(clippy::module_inception)]
mod linklet;
mod registry;

pub use directory::LinkletDirectory;
pub use effects::{PrimitiveEffect, primitive_effect, primitive_effects};
pub use instance::LibraryInstance;
pub use linklet::Linklet;
pub use registry::*;
