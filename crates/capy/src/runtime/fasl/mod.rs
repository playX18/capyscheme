//! FASL (Fast Loading) format
//!
//! Serializes/deserializes Scheme values to/from a binary format
//! that can be loaded quickly at runtime.

use std::io;

pub mod reader;
pub mod writer;

pub use reader::{LoadOptions, Reader};
pub use writer::{Compression, Image, Writer};

pub const FASL_EOF: u8 = 0;
pub const FASL_TAG_LOOKUP: u8 = 1;
pub const FASL_TAG_FIXNUM: u8 = 2;
pub const FASL_TAG_PLIST: u8 = 3;
pub const FASL_TAG_DLIST: u8 = 4;
pub const FASL_TAG_VECTOR: u8 = 5;
pub const FASL_TAG_RATIONAL: u8 = 6;
pub const FASL_TAG_COMPLEX: u8 = 7;
pub const FASL_TAG_FLONUM: u8 = 8;
pub const FASL_TAG_BIGINT: u8 = 9;
pub const FASL_TAG_BVECTOR: u8 = 10;
pub const FASL_TAG_CHAR: u8 = 11;
pub const FASL_TAG_NIL: u8 = 12;
pub const FASL_TAG_T: u8 = 13;
pub const FASL_TAG_F: u8 = 14;
pub const FASL_TAG_SYMBOL: u8 = 15;
pub const FASL_TAG_STR: u8 = 16;
pub const FASL_TAG_UNINTERNED_SYMBOL: u8 = 17;
pub const FASL_TAG_IMMEDIATE: u8 = 18;
pub const FASL_TAG_SYNTAX: u8 = 19;
pub const FASL_TAG_TUPLE: u8 = 20;
pub const FASL_TAG_KEYWORD: u8 = 21;
// 22-27 are reserved
pub const FASL_TAG_BEGIN: u8 = 28;
pub const FASL_TAG_PROGRAM: u8 = 30;

pub const FASL_COMPRESSION_NONE: u8 = 0;
pub const FASL_COMPRESSION_GZIP: u8 = 1;

/// Reference to an object.
pub const FASL_TAG_REF: u8 = 0xFF;
pub const FASL_TAG_REF_INIT: u8 = 0xFE;

pub const FASL_MAGIC: &[u8; 8] = b"CAPYFSL\0";
pub const FASL_VERSION: u32 = 13;
pub const MIN_SUPPORTED_FASL_VERSION: u32 = 11;

#[derive(Clone, Copy)]
pub enum ConstSpec<'gc> {
    Value(crate::runtime::value::Value<'gc>),
    Code(u32),
    Cell(u32),
}

pub struct CodeSpec<'a, 'gc> {
    pub code: &'a [u8],
    pub pool: &'a [u32],
    pub metadata_counts: &'a [u32],
    pub constants: &'a [ConstSpec<'gc>],
    pub arity: i32,
    pub frame_size: u32,
    pub metadata: crate::runtime::value::Value<'gc>,
    /// Deduplicated span values referenced by [`CodeSpec::srcmap`].
    pub spans: &'a [crate::runtime::value::Value<'gc>],
    /// Source runs in increasing byte-offset order.
    pub srcmap: &'a [crate::compiler::bytecode::lower::SrcRun],
}

pub struct ProgramSpec<'a, 'gc> {
    pub code_blocks: &'a [CodeSpec<'a, 'gc>],
    /// Index of the code block the loaded thunk runs.
    pub entry: u32,
}

pub(crate) fn checked_u32_len(len: usize) -> io::Result<u32> {
    u32::try_from(len)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "FASL payload is too large"))
}
