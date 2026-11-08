//! Machine-level LIR of the Tier 2 optimizer.
//!
//!
//!
//! - [`lower`]: IR to LIR conversion
//! - [`schedule`]: placement of floating pure values.

// `entity_impl!` expands to `std::u32::MAX`.
#![allow(deprecated)]
#![allow(unused_imports)]

pub mod lower;
pub mod schedule;

use std::fmt::{self, Display, Formatter};

use cranelift_entity::{PrimaryMap, entity_impl};

use crate::runtime::codegen::opt::ir::frame_state::FrameState;
use crate::runtime::codegen::opt::ir::func::{Block, Value};
use crate::runtime::codegen::opt::ir::inst::{
    Cmp, Constant, HelperRef, PrimRef, RematKind, ReturnValues, SwitchDensity,
};
use crate::runtime::codegen::opt::ir::types::{ClassRef, CodeRef, Type};

mod block;
mod func;
mod inst;
mod print;
mod types;

pub use crate::runtime::code::GuardKind;
pub use block::*;
pub use func::*;
pub use inst::*;
pub use types::*;
