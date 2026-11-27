//! FASL (Fast Loading) format
//!
//! Serializes/deserializes Scheme values to/from a binary format
//! that can be loaded quickly at runtime.

use std::io;

pub mod program_datum;
pub mod reader;
pub mod stats;
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
pub const FASL_TAG_RTD: u8 = 22;
pub const FASL_TAG_RCD: u8 = 23;
// 24-27 are reserved
pub const FASL_TAG_BEGIN: u8 = 28;
pub const FASL_TAG_PROGRAM: u8 = 30;
/// Marks the directory header section, see [`Writer::with_directory`].
pub const FASL_TAG_DIRECTORY: u8 = 31;

pub const FASL_COMPRESSION_NONE: u8 = 0;
pub const FASL_COMPRESSION_GZIP: u8 = 1;

/// Reference to an object.
pub const FASL_TAG_REF: u8 = 0xFF;
pub const FASL_TAG_REF_INIT: u8 = 0xFE;

/// The last byte is the layout version: 2 added the directory header section; 3 added the flags
/// byte after the compression byte and the SHA-256 trailer over everything after the magic.
pub const FASL_MAGIC: &[u8; 8] = b"CAPYFSL\x03";

/// Offset of the compression byte after the magic; the flags byte follows it.
const FASL_FLAGS_OFFSET: usize = FASL_MAGIC.len() + 1;
/// Flags byte: the image is a static bundle (its directory names `(boot static)`).
pub const FASL_FLAG_STATIC_BUNDLE: u8 = 1;
/// Length of the digest trailer.
pub const FASL_DIGEST_LEN: usize = 32;

pub fn fasl_payload(bytes: &[u8]) -> &[u8] {
    if bytes.starts_with(b"#!")
        && let Some(newline) = bytes.iter().position(|&byte| byte == b'\n')
    {
        return &bytes[newline + 1..];
    }
    bytes
}

pub fn is_fasl_image(bytes: &[u8]) -> bool {
    fasl_payload(bytes).starts_with(FASL_MAGIC)
}

pub fn is_static_bundle_image(bytes: &[u8]) -> bool {
    let payload = fasl_payload(bytes);
    payload.starts_with(FASL_MAGIC)
        && payload
            .get(FASL_FLAGS_OFFSET)
            .is_some_and(|flags| flags & FASL_FLAG_STATIC_BUNDLE != 0)
}

/// A run of bytecode bytes that came from one source span: the bytes
/// `[start, start + len)` of a code block, carrying a span id into the block's span table
/// (1-based, 0 = none) and `SRC_FLAG_*` flags.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SrcRun {
    pub start: u32,
    pub len: u32,
    pub flags: u8,
    pub span: u32,
}

#[derive(Clone, Copy)]
pub enum ConstSpec<'gc> {
    Value(crate::runtime::objectmodel::Value<'gc>),
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
    pub metadata: crate::runtime::objectmodel::Value<'gc>,
    /// Deduplicated span values referenced by [`CodeSpec::srcmap`].
    pub spans: &'a [crate::runtime::objectmodel::Value<'gc>],
    /// Source runs in increasing byte-offset order.
    pub srcmap: &'a [SrcRun],
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
