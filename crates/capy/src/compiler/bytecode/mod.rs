//! Bytecode backend: CFG procedures to code blocks.

pub mod disasm;
pub mod lower;

pub use lower::{CompiledProcedure, CompiledProgram, Constant, compile_program};
