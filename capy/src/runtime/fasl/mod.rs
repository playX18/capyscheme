//! FASL (Fast Loading) format
//!
//! Serializes/deserializes Scheme values to/from a binary format
//! that can be loaded quickly at runtime.
//!
//! ## Submodules
//!
//! - `writer` — `FaslWriter`: serializes Scheme values and code objects into FASL byte streams
//! - `reader` — `FaslReader`: deserializes FASL byte streams into live GC objects + JIT code
//! - `reloc` — `Relocation` / `RelocKind` / `RelocTarget`: relocation records
//! - `graph` — `FaslGraphTable`: graph tracking for shared structure and cycles

use std::io;

pub mod graph;
pub(crate) mod patch;
pub mod reader;
pub mod reloc;
pub mod writer;

pub use reader::FaslReader;
pub use writer::{FaslCompression, FaslImage, FaslWriter};

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
pub const FASL_TAG_CODE_BLOCK: u8 = 22;
pub const FASL_TAG_CLOSURE: u8 = 23;
pub const FASL_TAG_ENTRY: u8 = 24;
pub const FASL_TAG_GRAPH: u8 = 25;
pub const FASL_TAG_GRAPH_DEF: u8 = 26;
pub const FASL_TAG_GRAPH_REF: u8 = 27;
pub const FASL_TAG_BEGIN: u8 = 28;
pub const FASL_TAG_UNLINKED_CODEBLOCK: u8 = 29;

pub const FASL_COMPRESSION_NONE: u8 = 0;
pub const FASL_COMPRESSION_GZIP: u8 = 1;

pub const FASL_RELOC_ASMKIT: u8 = 0;
pub const FASL_RELOC_ABS_WORD: u8 = 1;
pub const FASL_RELOC_CODE_ENTRY: u8 = 2;
pub const FASL_RELOC_DATA_SLOT: u8 = 3;
pub const FASL_RELOC_RUNTIME_THUNK: u8 = 4;
pub const FASL_RELOC_RUNTIME_DATA: u8 = 5;
pub const FASL_RELOC_SIDE_METADATA: u8 = 6;
pub const FASL_RELOC_CACHE_CELL: u8 = 7;
pub const FASL_RELOC_CRANELIFT: u8 = 8;
pub const FASL_RELOC_CRANELIFT_DATA_SLOT: u8 = 9;

/// Reference to an object.
pub const FASL_TAG_REF: u8 = 0xFF;
pub const FASL_TAG_REF_INIT: u8 = 0xFE;

/// FASL file magic bytes.
pub const FASL_MAGIC: &[u8; 8] = b"CAPYFSL\0";
/// Current FASL file format version.
pub const FASL_VERSION: u32 = 4;

pub struct CodeSpec<'a, 'gc> {
    pub bytes: &'a [u8],
    pub entry_offset: u32,
    pub arity: i32,
    pub is_cont: bool,
    pub metadata: crate::runtime::value::Value<'gc>,
    pub relocations: &'a [reloc::Relocation],
}

impl<'a, 'gc> CodeSpec<'a, 'gc> {
    pub fn new(
        bytes: &'a [u8],
        entry_offset: u32,
        arity: i32,
        is_cont: bool,
        metadata: crate::runtime::value::Value<'gc>,
        relocations: &'a [reloc::Relocation],
    ) -> Self {
        Self {
            bytes,
            entry_offset,
            arity,
            is_cont,
            metadata,
            relocations,
        }
    }
}

pub struct ProgramSpec<'a, 'gc> {
    pub graph_len: u32,
    pub values: &'a [GraphValueSpec<'gc>],
    pub code_blocks: &'a [GraphCodeSpec<'a, 'gc>],
    pub entry_code_index: u32,
    pub entry_is_cont: bool,
}

impl<'a, 'gc> ProgramSpec<'a, 'gc> {
    pub fn new(
        graph_len: u32,
        values: &'a [GraphValueSpec<'gc>],
        code_blocks: &'a [GraphCodeSpec<'a, 'gc>],
        entry_code_index: u32,
        entry_is_cont: bool,
    ) -> Self {
        Self {
            graph_len,
            values,
            code_blocks,
            entry_code_index,
            entry_is_cont,
        }
    }
}

pub struct GraphValueSpec<'gc> {
    pub index: u32,
    pub value: crate::runtime::value::Value<'gc>,
}

impl<'gc> GraphValueSpec<'gc> {
    pub fn new(index: u32, value: crate::runtime::value::Value<'gc>) -> Self {
        Self { index, value }
    }
}

pub struct GraphCodeSpec<'a, 'gc> {
    pub index: u32,
    pub code: CodeSpec<'a, 'gc>,
}

impl<'a, 'gc> GraphCodeSpec<'a, 'gc> {
    pub fn new(index: u32, code: CodeSpec<'a, 'gc>) -> Self {
        Self { index, code }
    }
}

pub(crate) fn checked_u32_len(len: usize) -> io::Result<u32> {
    u32::try_from(len)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "FASL payload is too large"))
}

#[cfg(test)]
mod tests;
