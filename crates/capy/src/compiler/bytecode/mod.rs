//! Bytecode backend: CFG procedures to code blocks.

pub mod disasm;
pub mod lower;

pub use lower::{Constant, compile_program};
