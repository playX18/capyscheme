//! FASL (Fast Loading) format
//!
//! Serializes/deserializes Scheme values to/from a binary format
//! that can be loaded quickly at runtime.
//!
//! ## Submodules
//!
//! - `writer` — `FASLWriter`: serializes Scheme values and code objects into FASL byte streams
//! - `reader` — `FASLReader`: deserializes FASL byte streams into live GC objects + JIT code
//! - `reloc` — `FaslRelocation` / `FaslRelocKind` / `FaslRelocTarget`: relocation records
//! - `graph` — `FaslGraphTable`: graph tracking for shared structure and cycles

use std::io;

pub mod graph;
pub mod reader;
pub mod reloc;
pub mod writer;

pub use reader::FASLReader;
pub use writer::FASLWriter;

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
pub const FASL_TAG_GZIP: u8 = 28;
pub const FASL_TAG_LZ4: u8 = 29;
pub const FASL_TAG_BEGIN: u8 = 30;
pub const FASL_TAG_GROUP: u8 = 31;
pub const FASL_TAG_UNCOMPRESSED: u8 = 32;

pub const FASL_SITUATION_VISIT: u8 = 0;
pub const FASL_SITUATION_REVISIT: u8 = 1;
pub const FASL_SITUATION_VISIT_REVISIT: u8 = 2;

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
pub const FASL_VERSION: u32 = 1;

pub struct FaslCodeBlockSpec<'a, 'gc> {
    pub bytes: &'a [u8],
    pub entry_offset: u32,
    pub arity: i32,
    pub is_cont: bool,
    pub metadata: crate::runtime::value::Value<'gc>,
    pub relocations: &'a [reloc::FaslRelocation],
}

pub struct FaslClosureSpec<'a, 'gc> {
    pub code: FaslCodeBlockSpec<'a, 'gc>,
    pub free: &'a [crate::runtime::value::Value<'gc>],
    pub is_cont: bool,
}

pub struct FaslProgramSpec<'a, 'gc> {
    pub graph_len: u32,
    pub values: &'a [FaslGraphValueSpec<'gc>],
    pub code_blocks: &'a [FaslGraphCodeBlockSpec<'a, 'gc>],
    pub entry_code_index: u32,
    pub entry_is_cont: bool,
}

pub struct FaslGraphValueSpec<'gc> {
    pub index: u32,
    pub value: crate::runtime::value::Value<'gc>,
}
pub struct FaslGraphCodeBlockSpec<'a, 'gc> {
    pub index: u32,
    pub code: FaslCodeBlockSpec<'a, 'gc>,
}

pub(crate) fn checked_u32_len(len: usize) -> io::Result<u32> {
    u32::try_from(len)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "FASL payload is too large"))
}

#[cfg(test)]
mod tests;
