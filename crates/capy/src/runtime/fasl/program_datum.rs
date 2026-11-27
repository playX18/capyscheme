//! Program images as Scheme data.
//!
//! A *program datum* is the Scheme view of a [`ProgramSpec`], what `fasl-read` returns in
//! `'program-datum` mode and what `fasl-write` takes in place of a program built in Rust:
//!
//! ```text
//! #(#%program entry #(code ...))
//! code     = #(arity frame-size metadata code-bv pool-bv metadata-counts constants spans srcmap)
//! ```
//!
//! - `#%program`: the symbol named `#%program`, so `(string->symbol "#%program")`.
//! - `entry`: fixnum, the index in the code vector of the block the loaded thunk runs.
//! - `arity`: fixnum, the raw `CodeArity` value of the block (may be negative).
//! - `frame-size`: fixnum.
//! - `metadata`: any datum, the block's `info` (an alist, or `#f`).
//! - `code-bv`: bytevector, the encoded instructions.
//! - `pool-bv`: bytevector holding the aux pool as `u32` words, 4 bytes each, little endian.
//! - `metadata-counts`: vector of `NUM_METADATA_KINDS` fixnums, the per-kind metadata entry
//!   counts (see `%bytecode-isa`).
//! - `constants`: vector of pairs, in constant-pool order: `(value . v)` a literal,
//!   `(code . i)` the closure code of block `i` of this program, `(cell . id)` the cache cell
//!   `id`. Cells are numbered across the whole program, so blocks naming the same `id` share
//!   one cell.
//! - `spans`: vector of source span values. Span id `n` in a source run is `spans[n - 1]`, and
//!   id 0 means no span.
//! - `srcmap`: vector of `#(start len flags span)`, fixnums, runs in increasing `start` order.
//!   `flags` is the `SRC_FLAG_*` bits and `span` an id into `spans`.

use std::io;

use crate::bytecode::def;
use crate::runtime::MutationContext;
use crate::runtime::objectmodel::{ByteVector, Symbol, Value, Vector};

use super::{CodeSpec, ConstSpec, ProgramSpec, SrcRun};

/// The name of the symbol that marks a program datum.
pub const PROGRAM_TAG: &str = "#%program";

const CODE_FIELDS: usize = 9;

fn fixnum<'gc>(value: u32, what: &str) -> io::Result<Value<'gc>> {
    i32::try_from(value)
        .map(Value::new)
        .map_err(|_| invalid(format!("{what} {value} is too large for a fixnum")))
}

fn invalid(message: impl Into<String>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message.into())
}

fn vector<'gc>(ctx: MutationContext<'gc>, items: &[Value<'gc>]) -> Value<'gc> {
    Vector::from_slice(ctx, items).into()
}

/// The program datum of `spec`.
pub fn program_to_datum<'gc>(
    ctx: MutationContext<'gc>,
    spec: &ProgramSpec<'_, 'gc>,
) -> io::Result<Value<'gc>> {
    let mut codes = Vec::with_capacity(spec.code_blocks.len());
    for code in spec.code_blocks {
        codes.push(code_to_datum(ctx, code)?);
    }
    Ok(vector(
        ctx,
        &[
            Symbol::from_str(ctx, PROGRAM_TAG).into(),
            fixnum(spec.entry, "entry")?,
            vector(ctx, &codes),
        ],
    ))
}

fn code_to_datum<'gc>(
    ctx: MutationContext<'gc>,
    code: &CodeSpec<'_, 'gc>,
) -> io::Result<Value<'gc>> {
    let pool_bytes: Vec<u8> = code
        .pool
        .iter()
        .flat_map(|word| word.to_le_bytes())
        .collect();
    let counts = code
        .metadata_counts
        .iter()
        .map(|count| fixnum(*count, "metadata count"))
        .collect::<io::Result<Vec<_>>>()?;
    let tag = |name: &str| Value::from(Symbol::from_str(ctx, name));
    let constants = code
        .constants
        .iter()
        .map(|constant| {
            Ok(match constant {
                ConstSpec::Value(value) => Value::cons(ctx, tag("value"), *value),
                ConstSpec::Code(index) => Value::cons(ctx, tag("code"), fixnum(*index, "code")?),
                ConstSpec::Cell(id) => Value::cons(ctx, tag("cell"), fixnum(*id, "cell")?),
            })
        })
        .collect::<io::Result<Vec<_>>>()?;
    let srcmap = code
        .srcmap
        .iter()
        .map(|run| {
            Ok(vector(
                ctx,
                &[
                    fixnum(run.start, "source run start")?,
                    fixnum(run.len, "source run length")?,
                    Value::new(i32::from(run.flags)),
                    fixnum(run.span, "span id")?,
                ],
            ))
        })
        .collect::<io::Result<Vec<_>>>()?;
    Ok(vector(
        ctx,
        &[
            Value::new(code.arity),
            fixnum(code.frame_size, "frame size")?,
            code.metadata,
            ByteVector::from_slice(ctx, code.code, true).into(),
            ByteVector::from_slice(ctx, &pool_bytes, true).into(),
            vector(ctx, &counts),
            vector(ctx, &constants),
            vector(ctx, code.spans),
            vector(ctx, &srcmap),
        ],
    ))
}

/// A [`CodeSpec`] that owns its parts.
pub struct OwnedCode<'gc> {
    pub code: Vec<u8>,
    pub pool: Vec<u32>,
    pub metadata_counts: Vec<u32>,
    pub constants: Vec<ConstSpec<'gc>>,
    pub arity: i32,
    pub frame_size: u32,
    pub metadata: Value<'gc>,
    pub spans: Vec<Value<'gc>>,
    pub srcmap: Vec<SrcRun>,
}

/// A [`ProgramSpec`] that owns its parts, read from a program datum.
pub struct OwnedProgram<'gc> {
    pub codes: Vec<OwnedCode<'gc>>,
    pub entry: u32,
}

impl<'gc> OwnedProgram<'gc> {
    /// Calls `f` with the program as a [`ProgramSpec`].
    pub fn with_spec<R>(&self, f: impl FnOnce(&ProgramSpec<'_, 'gc>) -> R) -> R {
        let blocks: Vec<CodeSpec<'_, 'gc>> = self
            .codes
            .iter()
            .map(|code| CodeSpec {
                code: &code.code,
                pool: &code.pool,
                metadata_counts: &code.metadata_counts,
                constants: &code.constants,
                arity: code.arity,
                frame_size: code.frame_size,
                metadata: code.metadata,
                spans: &code.spans,
                srcmap: &code.srcmap,
            })
            .collect();
        f(&ProgramSpec {
            code_blocks: &blocks,
            entry: self.entry,
        })
    }
}

/// Whether programs the compiler builds are verified before they are written, and when a program
/// datum is read: always in a debug build, and with `CAPY_VERIFY_BYTECODE=1` (CI sets it). A
/// program read from an image is always verified, see the FASL reader.
pub fn verify_enabled() -> bool {
    cfg!(debug_assertions) || crate::utils::flags::verify_bytecode()
}

/// Checks every code block of `program` with the bytecode verifier.
pub fn verify_program(program: &OwnedProgram<'_>) -> Result<(), String> {
    for (index, block) in program.codes.iter().enumerate() {
        let counts: [u32; def::NUM_METADATA_KINDS] =
            block.metadata_counts.as_slice().try_into().map_err(|_| {
                format!("code block {index} has the wrong number of metadata counts")
            })?;
        crate::bytecode::verify::verify(&crate::bytecode::verify::CodeShape {
            code: &block.code,
            pool: &block.pool,
            nconsts: block.constants.len(),
            frame_size: block.frame_size as usize,
            metadata_counts: &counts,
        })
        .map_err(|error| format!("code block {index} does not verify: {error}"))?;
    }
    Ok(())
}

/// Whether `datum` is a vector whose first element is the `#%program` symbol.
pub fn is_program_datum(datum: Value<'_>) -> bool {
    datum
        .try_as::<Vector>()
        .and_then(|vector| vector.as_slice().first().copied())
        .and_then(|tag| tag.try_as::<Symbol>())
        .is_some_and(|symbol| symbol.to_string() == PROGRAM_TAG)
}

fn u32_of(value: Value<'_>, what: &str) -> Result<u32, String> {
    value
        .int32()
        .and_then(|n| u32::try_from(n).ok())
        .ok_or_else(|| format!("{what} must be a non-negative fixnum"))
}

fn vector_of<'gc>(value: Value<'gc>, what: &str) -> Result<&'gc [Value<'gc>], String> {
    value
        .try_as::<Vector>()
        .map(|vector| vector.as_gc_ref().as_slice())
        .ok_or_else(|| format!("{what} must be a vector"))
}

/// Reads a program datum. The error is a message about the first malformed part.
pub fn datum_to_program<'gc>(datum: Value<'gc>) -> Result<OwnedProgram<'gc>, String> {
    if !is_program_datum(datum) {
        return Err("not a #%program datum".into());
    }
    let top = vector_of(datum, "program")?;
    if top.len() != 3 {
        return Err("program datum must be #(#%program entry #(code ...))".into());
    }
    let entry = u32_of(top[1], "program entry")?;
    let blocks = vector_of(top[2], "program code vector")?;
    if entry as usize >= blocks.len() {
        return Err("program entry is out of range".into());
    }
    let mut codes = Vec::with_capacity(blocks.len());
    for (index, block) in blocks.iter().enumerate() {
        codes.push(
            datum_to_code(*block, blocks.len())
                .map_err(|message| format!("code {index}: {message}"))?,
        );
    }
    Ok(OwnedProgram { codes, entry })
}

fn datum_to_code<'gc>(block: Value<'gc>, ncodes: usize) -> Result<OwnedCode<'gc>, String> {
    let fields = vector_of(block, "code")?;
    if fields.len() != CODE_FIELDS {
        return Err(format!("code must have {CODE_FIELDS} fields"));
    }
    let arity = fields[0]
        .int32()
        .ok_or_else(|| "arity must be a fixnum".to_string())?;
    let frame_size = u32_of(fields[1], "frame size")?;
    let metadata = fields[2];
    let code = fields[3]
        .try_as::<ByteVector>()
        .ok_or_else(|| "code must be a bytevector".to_string())?
        .as_slice()
        .to_vec();
    let pool_bytes = fields[4]
        .try_as::<ByteVector>()
        .ok_or_else(|| "pool must be a bytevector".to_string())?
        .as_slice();
    if pool_bytes.len() % 4 != 0 {
        return Err("pool bytevector length must be a multiple of 4".into());
    }
    let pool = pool_bytes
        .as_chunks::<4>().0.iter()
        .map(|word| u32::from_le_bytes([word[0], word[1], word[2], word[3]]))
        .collect();
    let counts = vector_of(fields[5], "metadata counts")?;
    if counts.len() != def::NUM_METADATA_KINDS {
        return Err(format!(
            "metadata counts must have {} entries",
            def::NUM_METADATA_KINDS
        ));
    }
    let metadata_counts = counts
        .iter()
        .map(|count| u32_of(*count, "metadata count"))
        .collect::<Result<Vec<_>, _>>()?;

    let mut constants = Vec::new();
    for constant in vector_of(fields[6], "constants")? {
        if !constant.is_pair() {
            return Err("a constant must be a pair (kind . payload)".into());
        }
        let kind = constant
            .car()
            .try_as::<Symbol>()
            .map(|symbol| symbol.to_string())
            .unwrap_or_default();
        constants.push(match kind.as_str() {
            "value" => ConstSpec::Value(constant.cdr()),
            "code" => {
                let index = u32_of(constant.cdr(), "code constant index")?;
                if index as usize >= ncodes {
                    return Err("code constant index is out of range".into());
                }
                ConstSpec::Code(index)
            }
            "cell" => ConstSpec::Cell(u32_of(constant.cdr(), "cell constant id")?),
            _ => return Err("constant kind must be value, code or cell".into()),
        });
    }

    let spans = vector_of(fields[7], "spans")?.to_vec();
    let mut srcmap = Vec::new();
    for run in vector_of(fields[8], "srcmap")? {
        let run = vector_of(*run, "source run")?;
        if run.len() != 4 {
            return Err("a source run must be #(start len flags span)".into());
        }
        let flags = u32_of(run[2], "source run flags")?;
        let flags = u8::try_from(flags).map_err(|_| "source run flags must fit a byte")?;
        let span = u32_of(run[3], "source run span")?;
        if span as usize > spans.len() {
            return Err("source run span id is out of range".into());
        }
        srcmap.push(SrcRun {
            start: u32_of(run[0], "source run start")?,
            len: u32_of(run[1], "source run length")?,
            flags,
            span,
        });
    }

    Ok(OwnedCode {
        code,
        pool,
        metadata_counts,
        constants,
        arity,
        frame_size,
        metadata,
        spans,
        srcmap,
    })
}
