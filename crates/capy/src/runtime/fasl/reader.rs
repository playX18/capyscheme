use super::*;
use crate::bytecode::{def, verify};
use crate::heap::{Gc, Global, Trace, Visitor, mmtk::util::Address, sync::monitor::Monitor};
use crate::runtime::{MutationContext, value::*, vm::syntax::Syntax};
use flate2::read::GzDecoder;
use std::{
    collections::HashMap,
    io::{self, BufReader, Cursor, Read},
};

pub struct Reader<'gc, R: io::Read> {
    pub ctx: MutationContext<'gc>,
    pub reader: BufReader<R>,
    pub lites: Gc<'gc, HashTable<'gc>>,
    options: LoadOptions,
    roots: Global<crate::Rootable!(Roots<'_>)>,
    pub reference_map: HashMap<u32, Address>,
    format_version: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LoadOptions {
    pub debug_entries: bool,
}

impl LoadOptions {
    pub const NORMAL: Self = Self {
        debug_entries: false,
    };
    pub const DEBUG: Self = Self {
        debug_entries: true,
    };

    /// Options from the `fasl_debug` flag (`CAPY_FASL_DEBUG`).
    pub fn from_flags() -> Self {
        if crate::utils::flags::fasl_debug() {
            Self::DEBUG
        } else {
            Self::NORMAL
        }
    }
}

struct Roots<'gc> {
    values: Monitor<Vec<Value<'gc>>>,
    fill_code_blocks: Monitor<Vec<Value<'gc>>>,
}

// SAFETY: `gc` for `Roots` upholds all trait invariants
unsafe impl<'gc> Trace for Roots<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for value in self.values.get_mut().iter_mut() {
            visitor.trace(value);
        }
        for value in self.fill_code_blocks.get_mut().iter_mut() {
            visitor.trace(value);
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::heap::WeakProcessor) {}
}

// SAFETY: `Roots` is `Send` because all mutable state is synchronized
unsafe impl Send for Roots<'_> {}
// SAFETY: `Roots` is `Sync` because all mutable access is serialized
unsafe impl Sync for Roots<'_> {}

/// A constant-pool entry as read, before code blocks exist to point at.
enum ReadConst<'gc> {
    Value(Value<'gc>),
    Code(u32),
    Cell(u32),
}

struct ReadCode<'gc> {
    code: Vec<u8>,
    pool: Vec<u32>,
    metadata_counts: [u32; def::NUM_METADATA_KINDS],
    constants: Vec<ReadConst<'gc>>,
    arity: i32,
    frame_size: u32,
    metadata: Value<'gc>,
    spans: Vec<Value<'gc>>,
    srcmap: Vec<(u32, u32, u8, u32)>,
}

fn invalid(message: impl Into<String>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message.into())
}

impl<'gc, R: io::Read> Reader<'gc, R> {
    pub fn read8(&mut self) -> io::Result<u8> {
        let mut buf = [0; 1];
        self.reader.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    pub fn read32(&mut self) -> io::Result<u32> {
        let mut buf = [0; 4];
        self.reader.read_exact(&mut buf)?;
        Ok(u32::from_le_bytes(buf))
    }

    pub fn read64(&mut self) -> io::Result<u64> {
        let mut buf = [0; 8];
        self.reader.read_exact(&mut buf)?;
        Ok(u64::from_le_bytes(buf))
    }

    fn read_uleb128(&mut self) -> io::Result<u32> {
        let mut result = 0u32;
        for shift in [0, 7, 14, 21, 28] {
            let byte = self.read8()?;
            let payload = (byte & 0x7f) as u32;
            if shift == 28 {
                if payload > 0x0f || byte & 0x80 != 0 {
                    return Err(invalid("FASL uleb128 overflow"));
                }
                return Ok(result | (payload << 28));
            }
            result |= payload << shift;
            if byte & 0x80 == 0 {
                return Ok(result);
            }
        }
        unreachable!("FASL uleb128 always terminates by the fifth byte");
    }

    pub fn read_lites(&mut self) -> io::Result<()> {
        let count = self.read32()? as usize;

        for _ in 0..count {
            let id = self.read32()? as i32;
            let tag = self.read8()?;

            let key = match tag {
                FASL_TAG_SYMBOL => {
                    let len = self.read32()? as usize;
                    let mut buf = vec![0; len];
                    self.reader.read_exact(&mut buf)?;
                    let str = std::str::from_utf8(&buf).map_err(|_| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("Invalid UTF-8 (symbol): {}", String::from_utf8_lossy(&buf)),
                        )
                    })?;

                    Value::new(Symbol::from_str(self.ctx, str))
                }
                FASL_TAG_UNINTERNED_SYMBOL => {
                    let len = self.read32()? as usize;
                    let mut buf = vec![0; len];
                    self.reader.read_exact(&mut buf)?;
                    let str = std::str::from_utf8(&buf).map_err(|_| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!(
                                "Invalid UTF-8 (uninterned symbol): {}",
                                String::from_utf8_lossy(&buf)
                            ),
                        )
                    })?;

                    Value::new(Symbol::from_str_uninterned(self.ctx, str, None))
                }
                FASL_TAG_STR => {
                    let len = self.read32()? as usize;
                    let mut buf = vec![0u8; len];
                    self.reader.read_exact(&mut buf)?;
                    let str = std::str::from_utf8(&buf).map_err(|_| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            format!("Invalid UTF-8 (string): {}", String::from_utf8_lossy(&buf)),
                        )
                    })?;
                    // Decoded constants are immutable (R6RS string literals),
                    // the encoding carries no mutability bit and fasl only
                    // stores compiler artifacts, never user data.
                    Value::new(Str::new(self.ctx, str, true))
                }
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Invalid tag for lites",
                    ));
                }
            };

            self.lites.put(self.ctx, Value::new(id), key);
        }
        Ok(())
    }

    pub fn read_value(&mut self) -> io::Result<Value<'gc>> {
        let tag = self.read8()?;

        self.read_tagged_value(tag)
    }

    fn read_tagged_value(&mut self, tag: u8) -> io::Result<Value<'gc>> {
        match tag {
            _x @ FASL_TAG_FIXNUM => {
                let value = self.read32()? as i32;
                Ok(Value::new(value))
            }
            _x @ FASL_TAG_FLONUM => {
                let bits = self.read64()?;
                Ok(Value::new(f64::from_bits(bits)))
            }
            _x @ FASL_TAG_T => Ok(Value::new(true)),
            _x @ FASL_TAG_F => Ok(Value::new(false)),
            _x @ FASL_TAG_NIL => Ok(Value::null()),
            _x @ FASL_TAG_CHAR => {
                let value = self.read32()?;
                Ok(Value::new(char::from_u32(value).ok_or_else(|| {
                    io::Error::new(io::ErrorKind::InvalidData, "Invalid char value")
                })?))
            }
            _x @ FASL_TAG_IMMEDIATE => {
                let value = self.read64()?;
                Ok(Value::from_raw_i64(value as i64))
            }

            _x @ FASL_TAG_REF => {
                let ref_id = self.read32()?;
                if let Some(addr) = self.reference_map.get(&ref_id) {
                    Ok(Value::from_raw(addr.as_usize() as u64))
                } else {
                    Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("Unknown reference ID: {ref_id}"),
                    ))
                }
            }

            _x @ FASL_TAG_REF_INIT => {
                let ref_id = self.read32()?;
                let value = self.read_value()?;
                self.reference_map
                    .insert(ref_id, value.as_cell_raw().to_address());
                Ok(value)
            }

            _x @ FASL_TAG_PLIST => {
                let count = self.read32()? as usize;
                let mut lst = Value::null();
                for _ in 0..count {
                    lst = Value::cons(self.ctx, self.read_value()?, lst);
                }

                Ok(lst)
            }

            _x @ FASL_TAG_DLIST => {
                let count = self.read32()? as usize;
                let mut lst = self.read_value()?;

                for _ in 0..count {
                    lst = Value::cons(self.ctx, self.read_value()?, lst);
                }

                Ok(lst)
            }

            _x @ FASL_TAG_VECTOR => {
                let count = self.read32()? as usize;
                let mut vec = Vec::with_capacity(count);

                for _ in 0..count {
                    vec.push(self.read_value()?);
                }
                Ok(Value::new(Vector::from_slice(self.ctx, &vec)))
            }

            _x @ FASL_TAG_TUPLE => {
                let count = self.read32()? as usize;
                let mut vec = Vec::with_capacity(count);

                for _ in 0..count {
                    vec.push(self.read_value()?);
                }
                Ok(Value::new(Tuple::from_slice(self.ctx, &vec)))
            }

            _x @ FASL_TAG_BVECTOR => {
                let count = self.read64()? as usize;
                let mut buf = vec![0u8; count];
                self.reader.read_exact(&mut buf)?;
                Ok(Value::new(ByteVector::from_slice(self.ctx, &buf, true)))
            }

            _x @ FASL_TAG_BIGINT => {
                let negative = self.read8()? != 0;
                let count = self.read32()? as usize;
                let mut digits = Vec::with_capacity(count);

                for _ in 0..count {
                    digits.push(self.read64()?);
                }
                Ok(Value::new(BigInt::new::<false>(
                    self.ctx, &digits, negative,
                )))
            }

            _x @ FASL_TAG_RATIONAL => {
                let numerator = self.read_value()?.number().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Expected a number for numerator",
                    )
                })?;
                let denominator = self.read_value()?.number().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Expected a number for denominator",
                    )
                })?;
                Ok(Value::new(Rational::new(self.ctx, numerator, denominator)))
            }

            _x @ FASL_TAG_COMPLEX => {
                let value = self.read_value()?;
                let real = value.number().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("Expected a number for real part, got: {value}"),
                    )
                })?;
                let imag = self.read_value()?.number().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Expected a number for imaginary part",
                    )
                })?;

                Ok(Value::new(Complex::new(self.ctx, real, imag)))
            }

            _x @ FASL_TAG_LOOKUP => {
                let uid = self.read32()? as i32;

                if let Some(value) = self.lites.get(self.ctx, Value::new(uid)) {
                    Ok(value)
                } else {
                    Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Unknown lookup ID",
                    ))
                }
            }

            _x @ FASL_TAG_SYNTAX => {
                let expr = self.read_value()?;
                let module = self.read_value()?;
                let source = self.read_value()?;
                let wrap = self.read_value()?;
                let properties = self.read_value()?;
                Ok(Value::new(Syntax::new(
                    self.ctx, expr, wrap, module, source, properties,
                )))
            }

            _x @ FASL_TAG_KEYWORD => {
                let sym_value = self.read_value()?;
                let sym = sym_value.try_as::<Symbol>().ok_or_else(|| {
                    io::Error::new(io::ErrorKind::InvalidData, "Expected a symbol for keyword")
                })?;
                let map = self.ctx.globals().keyword_map.get().downcast::<HashTable>();
                if let Some(kw) = map.get(self.ctx, sym_value) {
                    return Ok(kw);
                }

                let kw = crate::runtime::value::Keyword::from_symbol(self.ctx, sym);
                self.ctx
                    .globals()
                    .keyword_map
                    .get()
                    .downcast::<HashTable>()
                    .put(self.ctx, sym_value, kw);
                Ok(kw.into())
            }

            _x @ FASL_TAG_PROGRAM => self.read_program(),

            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Unsupported tag for FASL deserialization",
            )),
        }
    }

    fn read_program(&mut self) -> io::Result<Value<'gc>> {
        let hash = self.read64()?;
        if hash != def::format_hash() {
            return Err(invalid(
                "FASL bytecode was compiled for a different instruction set",
            ));
        }

        const PREALLOC: usize = 4096;
        let count = self.read32()? as usize;
        let mut specs: Vec<ReadCode<'gc>> = Vec::with_capacity(count.min(PREALLOC));
        for _ in 0..count {
            let arity = self.read32()? as i32;
            let frame_size = self.read32()?;
            let metadata = self.read_value()?;
            let ncode = self.read32()? as usize;
            let mut code = Vec::with_capacity(ncode.min(PREALLOC));
            (&mut self.reader)
                .take(ncode as u64)
                .read_to_end(&mut code)?;
            if code.len() != ncode {
                return Err(io::Error::from(io::ErrorKind::UnexpectedEof));
            }
            let npool = self.read32()? as usize;
            let mut pool = Vec::with_capacity(npool.min(PREALLOC));
            for _ in 0..npool {
                pool.push(self.read32()?);
            }
            let nkinds = self.read32()? as usize;
            if nkinds != def::NUM_METADATA_KINDS {
                return Err(invalid("FASL metadata kind count mismatch"));
            }
            let mut metadata_counts = [0u32; def::NUM_METADATA_KINDS];
            for count in &mut metadata_counts {
                *count = self.read32()?;
            }
            let nconsts = self.read32()? as usize;
            let mut constants = Vec::with_capacity(nconsts.min(PREALLOC));
            for _ in 0..nconsts {
                constants.push(match self.read8()? {
                    0 => ReadConst::Value(self.read_value()?),
                    1 => ReadConst::Code(self.read32()?),
                    2 => ReadConst::Cell(self.read32()?),
                    _ => return Err(invalid("unknown FASL constant kind")),
                });
            }
            let nspans = self.read32()? as usize;
            let mut spans = Vec::with_capacity(nspans.min(PREALLOC));
            for _ in 0..nspans {
                spans.push(self.read_value()?);
            }
            let nruns = self.read32()? as usize;
            let mut srcmap = Vec::with_capacity(nruns.min(PREALLOC));
            let mut prev_start = 0u32;
            for _ in 0..nruns {
                let start = prev_start.wrapping_add(self.read_uleb128()?);
                let len = self.read_uleb128()?;
                let flags = self.read8()?;
                let span = self.read_uleb128()?;
                if span as usize > spans.len() {
                    return Err(invalid("FASL source span id out of range"));
                }
                srcmap.push((start, len, flags, span));
                prev_start = start;
            }
            specs.push(ReadCode {
                code,
                pool,
                metadata_counts,
                constants,
                arity,
                frame_size,
                metadata,
                spans,
                srcmap,
            });
        }
        let entry = self.read32()? as usize;
        if entry >= specs.len() {
            return Err(invalid("FASL entry code block index out of range"));
        }

        for spec in &specs {
            verify::verify(&verify::CodeShape {
                code: &spec.code,
                pool: &spec.pool,
                nconsts: spec.constants.len(),
                frame_size: spec.frame_size as usize,
                metadata_counts: &spec.metadata_counts,
            })
            .map_err(invalid)?;
            for constant in &spec.constants {
                if let ReadConst::Code(index) = constant
                    && *index as usize >= specs.len()
                {
                    return Err(invalid("FASL constant names a code block out of range"));
                }
            }
        }

        let ctx = self.ctx;
        let mut cells: HashMap<u32, Value<'gc>> = HashMap::new();
        let mut code_blocks: Vec<Gc<'gc, CodeBlock<'gc>>> = Vec::with_capacity(specs.len());
        for spec in &specs {
            let constants: Vec<Value<'gc>> = spec
                .constants
                .iter()
                .map(|constant| match constant {
                    ReadConst::Value(value) => *value,
                    // Patched below, once every code block exists.
                    ReadConst::Code(_) => Value::new(false),
                    ReadConst::Cell(id) => *cells.entry(*id).or_insert_with(|| {
                        crate::runtime::vm::thunks::make_variable(ctx, Value::new(false))
                    }),
                })
                .collect();
            let srcmap = if spec.srcmap.is_empty() {
                Value::new(false)
            } else {
                pack_srcmap(ctx, &spec.spans, &spec.srcmap)
            };
            code_blocks.push(CodeBlock::new_bytecode(
                ctx,
                CodeBlockInit {
                    code: &spec.code,
                    pool: &spec.pool,
                    metadata: def::MetadataLayout::new(&spec.metadata_counts),
                    constants: &constants,
                    arity: CodeArity::new(spec.arity),
                    frame_size: spec.frame_size,
                    info: spec.metadata,
                    srcmap,
                },
            ));
        }
        for (spec, code_block) in specs.iter().zip(code_blocks.iter()) {
            let mut patched = false;
            for (index, constant) in spec.constants.iter().enumerate() {
                if let ReadConst::Code(target) = constant {
                    // SAFETY: `index` is in range of the buffer built from
                    // the same constant list.
                    unsafe {
                        code_block
                            .buffer
                            .constant_slot(index)
                            .write(code_blocks[*target as usize].into());
                    }
                    patched = true;
                }
            }
            if patched {
                let _ = Gc::write(ctx, code_block.buffer);
            }
        }
        Ok(Closure::new(ctx, code_blocks[entry], 0, false).into())
    }

    pub fn read_header(&mut self) -> io::Result<()> {
        let mut magic = [0u8; 8];
        self.reader.read_exact(&mut magic)?;
        if &magic != FASL_MAGIC {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "invalid FASL magic: expected {:?}, got {:?}",
                    std::str::from_utf8(FASL_MAGIC).unwrap_or("<binary>"),
                    std::str::from_utf8(&magic).unwrap_or("<binary>"),
                ),
            ));
        }
        let version = self.read32()?;
        if !(MIN_SUPPORTED_FASL_VERSION..=FASL_VERSION).contains(&version) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "unsupported FASL version {version} (expected {MIN_SUPPORTED_FASL_VERSION}..={FASL_VERSION})"
                ),
            ));
        }
        self.format_version = version;
        Ok(())
    }

    pub fn read(mut self) -> io::Result<Value<'gc>> {
        self.read_header()?;
        let compression = self.read8()?;
        self.read_lites()?;
        let payload = self.read_payload(compression)?;
        let mut trailing = [0u8; 1];
        if self.reader.read(&mut trailing)? != 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL image has trailing bytes",
            ));
        }
        let value = self.read_payload_value(payload)?;
        self.keep_value(value);

        Ok(value)
    }

    pub fn new(ctx: MutationContext<'gc>, reader: R) -> Self {
        Self::new_with_options(ctx, reader, LoadOptions::NORMAL)
    }

    pub fn new_with_options(ctx: MutationContext<'gc>, reader: R, options: LoadOptions) -> Self {
        let lites = HashTable::new(ctx, HashTableType::Eq, 32, 0.75);
        let roots = Global::new(Roots {
            values: Monitor::new(vec![Value::from(lites)]),
            fill_code_blocks: Monitor::new(Vec::new()),
        });
        Self {
            ctx,
            reader: BufReader::new(reader),
            lites,
            options,
            roots,
            reference_map: HashMap::new(),
            format_version: 0,
        }
    }

    fn keep_value(&self, value: Value<'gc>) {
        self.roots.fetch(self.ctx).values.lock().push(value);
    }

    fn read_payload(&mut self, compression: u8) -> io::Result<Vec<u8>> {
        let uncompressed_size = self.read32()? as usize;
        let stored_size = self.read32()? as usize;
        let mut stored = vec![0; stored_size];
        self.reader.read_exact(&mut stored)?;
        let payload = match compression {
            FASL_COMPRESSION_NONE => stored,
            FASL_COMPRESSION_GZIP => {
                let mut decoder = GzDecoder::new(Cursor::new(stored));
                let mut payload = Vec::with_capacity(uncompressed_size);
                decoder.read_to_end(&mut payload)?;
                payload
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("unknown FASL compression tag {compression}"),
                ));
            }
        };
        if payload.len() != uncompressed_size {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL payload size mismatch",
            ));
        }
        Ok(payload)
    }

    fn read_payload_value(&self, payload: Vec<u8>) -> io::Result<Value<'gc>> {
        let mut reader = Reader::new_with_options(self.ctx, Cursor::new(payload), self.options);
        reader.lites = self.lites;
        reader.format_version = self.format_version;
        reader.keep_value(Value::from(reader.lites));
        let value = reader.read_value()?;
        reader.keep_value(value);
        let cursor_position = reader.reader.get_ref().position();
        let buffered = reader.reader.buffer().len() as u64;
        let consumed = cursor_position.saturating_sub(buffered);
        if consumed != reader.reader.get_ref().get_ref().len() as u64 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL payload has trailing bytes",
            ));
        }
        Ok(value)
    }
}
