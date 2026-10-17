//! Bytecode: instruction encoding and validation, nothing else.
//!
//! - [`def`]: opcode and operand definitions, the wire format.
//! - [`operand`]: operand types.
//! - [`verify`]: the bytecode verifier.
//!
//! Everything of runtime concern lives elsewhere: the engines and their
//! frames, stacks, and slow paths in
//! [`interp`](crate::runtime::interp), primitives in
//! [`prims`](crate::runtime::prims).

pub mod def;
pub mod operand;
pub mod verify;
