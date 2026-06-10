use std::{
    collections::{HashMap, hash_map::Entry},
    io::{self, BufReader, Cursor, Read},
};

use flate2::read::GzDecoder;

use crate::rsgc::{Gc, Global, Trace, Visitor, mmtk::util::Address, sync::monitor::Monitor};

use crate::runtime::{
    Context,
    code_memory::{CodeAllocation, runtime_code_memory},
    symbols::{RuntimeData, RuntimeThunk},
    value::{
        BigInt, ByteVector, Closure, CodeArity, CodeBlock, Complex, HashTable, HashTableType,
        LoadedCodeBlockInit, Pair, Rational, RelocatableCodeBlock, Str, Symbol, Tuple, Value,
        Vector,
    },
    vm::syntax::Syntax,
};

use super::{
    FASL_COMPRESSION_GZIP, FASL_COMPRESSION_NONE, FASL_MAGIC, FASL_TAG_BEGIN, FASL_TAG_BIGINT,
    FASL_TAG_BVECTOR, FASL_TAG_CHAR, FASL_TAG_CLOSURE, FASL_TAG_CODE_BLOCK, FASL_TAG_COMPLEX,
    FASL_TAG_DLIST, FASL_TAG_ENTRY, FASL_TAG_F, FASL_TAG_FIXNUM, FASL_TAG_FLONUM, FASL_TAG_GRAPH,
    FASL_TAG_GRAPH_DEF, FASL_TAG_GRAPH_REF, FASL_TAG_IMMEDIATE, FASL_TAG_KEYWORD, FASL_TAG_LOOKUP,
    FASL_TAG_NIL, FASL_TAG_PLIST, FASL_TAG_RATIONAL, FASL_TAG_REF, FASL_TAG_REF_INIT, FASL_TAG_STR,
    FASL_TAG_SYMBOL, FASL_TAG_SYNTAX, FASL_TAG_T, FASL_TAG_TUPLE, FASL_TAG_UNINTERNED_SYMBOL,
    FASL_TAG_UNLINKED_CODEBLOCK, FASL_TAG_VECTOR, FASL_VERSION, graph, patch, reloc,
};

pub struct FaslReader<'gc, R: io::Read> {
    pub ctx: Context<'gc>,
    pub reader: BufReader<R>,
    pub lites: Gc<'gc, HashTable<'gc>>,
    roots: Global<crate::Rootable!(FaslReaderRoots<'_>)>,
    pub reference_map: HashMap<u32, Address>,
    graph_stack: Vec<graph::FaslGraphTable<'gc>>,
    shared_cache_slots: Vec<HashMap<u32, SharedCacheSlot<'gc>>>,
    pending_code_entry_relocations: Vec<Vec<PendingCodeEntryRelocation>>,
    pending_data_slot_fills: Vec<Vec<PendingDataSlotFill>>,
    pending_code_entry_slot_fills: Vec<Vec<PendingCodeEntrySlotFill>>,
}

struct FaslReaderRoots<'gc> {
    values: Monitor<Vec<Value<'gc>>>,
}

// SAFETY: `gc` for `FaslReaderRoots` upholds all trait invariants
unsafe impl<'gc> Trace for FaslReaderRoots<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for value in self.values.get_mut().iter_mut() {
            visitor.trace(value);
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}
}

// SAFETY: `FaslReaderRoots` is `Send` because all mutable state is synchronized
unsafe impl Send for FaslReaderRoots<'_> {}
// SAFETY: `FaslReaderRoots` is `Sync` because all mutable access is serialized
unsafe impl Sync for FaslReaderRoots<'_> {}

#[derive(Clone, Copy)]
struct SharedCacheSlot<'gc> {
    address: Address,
    owner: Value<'gc>,
}

#[derive(Clone)]
struct PendingCodeEntryRelocation {
    source_index: u32,
    base: Address,
    offset: usize,
    target_index: u32,
    kind: reloc::RelocKind,
    addend: i64,
    original_bytes: Vec<u8>,
}

#[derive(Clone, Copy)]
struct PendingDataSlotFill {
    target_index: u32,
    slot_address: Address,
}

#[derive(Clone, Copy)]
struct PendingCodeEntrySlotFill {
    target_index: u32,
    slot_address: Address,
}

struct ReadCodeBlockSpec<'gc> {
    bytes: Vec<u8>,
    entry_offset: u32,
    arity: i32,
    is_cont: bool,
    metadata: Value<'gc>,
    relocations: Vec<reloc::Relocation>,
}

impl<'gc, R: io::Read> FaslReader<'gc, R> {
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

                    Value::new(Symbol::from_str_uninterned(*self.ctx, str, None))
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
                    Value::new(Str::new(*self.ctx, str, false))
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
                Ok(Value::new(Vector::from_slice(*self.ctx, &vec)))
            }

            _x @ FASL_TAG_TUPLE => {
                let count = self.read32()? as usize;
                let mut vec = Vec::with_capacity(count);

                for _ in 0..count {
                    vec.push(self.read_value()?);
                }
                Ok(Value::new(Tuple::from_slice(*self.ctx, &vec)))
            }

            _x @ FASL_TAG_BVECTOR => {
                let count = self.read64()? as usize;
                let mut buf = vec![0u8; count];
                self.reader.read_exact(&mut buf)?;
                Ok(Value::new(ByteVector::from_slice(*self.ctx, &buf, true)))
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

                let kw = crate::runtime::value::Keyword::from_symbol(*self.ctx, sym);
                self.ctx
                    .globals()
                    .keyword_map
                    .get()
                    .downcast::<HashTable>()
                    .put(self.ctx, sym_value, kw);
                Ok(kw.into())
            }

            _x @ FASL_TAG_GRAPH => {
                let graph_len = self.read32()? as usize;
                let external_count = self.read32()?;
                if external_count != 0 {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL graph external references are not supported yet",
                    ));
                }
                self.graph_stack.push(graph::FaslGraphTable::new(graph_len));
                self.shared_cache_slots.push(HashMap::new());
                self.pending_code_entry_relocations.push(Vec::new());
                self.pending_data_slot_fills.push(Vec::new());
                self.pending_code_entry_slot_fills.push(Vec::new());
                let value = self.read_value();
                let pending_entry_slots = self
                    .pending_code_entry_slot_fills
                    .pop()
                    .expect("pending code entry slot stack should match graph stack");
                let pending_data = self
                    .pending_data_slot_fills
                    .pop()
                    .expect("pending data slot stack should match graph stack");
                let pending = self
                    .pending_code_entry_relocations
                    .pop()
                    .expect("pending relocation stack should match graph stack");
                self.shared_cache_slots
                    .pop()
                    .expect("shared cache slot stack should match graph stack");
                self.graph_stack.pop();
                if value.is_ok()
                    && (!pending.is_empty()
                        || !pending_data.is_empty()
                        || !pending_entry_slots.is_empty())
                {
                    let detail = if !pending.is_empty() {
                        format!(
                            "unresolved FASL code entry relocation (target indices: {:?})",
                            pending.iter().map(|r| r.target_index).collect::<Vec<_>>()
                        )
                    } else if !pending_entry_slots.is_empty() {
                        format!(
                            "unresolved FASL code entry data slot fill (target indices: {:?})",
                            pending_entry_slots
                                .iter()
                                .map(|f| f.target_index)
                                .collect::<Vec<_>>()
                        )
                    } else {
                        format!(
                            "unresolved FASL data slot fill (target indices: {:?})",
                            pending_data
                                .iter()
                                .map(|f| f.target_index)
                                .collect::<Vec<_>>()
                        )
                    };
                    return Err(io::Error::new(io::ErrorKind::InvalidData, detail));
                }
                value
            }

            _x @ FASL_TAG_GRAPH_DEF => {
                let index = self.read32()?;
                self.read_graph_definition(index)
            }

            _x @ FASL_TAG_GRAPH_REF => {
                let index = self.read32()?;
                let graph = self.graph_stack.last().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL graph reference outside graph context",
                    )
                })?;
                graph.get(index)
            }

            _x @ FASL_TAG_BEGIN => {
                let count = self.read32()? as usize;
                if count == 0 {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL begin block is empty",
                    ));
                }
                let mut value = Value::undefined();
                for _ in 0..count {
                    value = self.read_value()?;
                }
                Ok(value)
            }

            _x @ FASL_TAG_ENTRY => {
                let index = self.read32()?;
                let graph = self.graph_stack.last().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL entry outside graph context",
                    )
                })?;
                let value = graph.get(index)?;
                let code_block = value.try_as::<CodeBlock>().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL entry target is not a code block",
                    )
                })?;
                Ok(Value::from(code_block))
            }

            _x @ FASL_TAG_CODE_BLOCK => {
                let spec = self.read_code_block_spec()?;
                Ok(Value::from(self.link_code_block_from_spec(None, spec)?))
            }

            _x @ FASL_TAG_UNLINKED_CODEBLOCK => {
                let unlinked = self.read_relocatable_code_block()?;
                Ok(Value::from(unlinked))
            }

            _x @ FASL_TAG_CLOSURE => {
                let code_block = self.read_closure_code_block()?;
                let free_count = self.read32()? as usize;
                let mut free = Vec::with_capacity(free_count);
                for _ in 0..free_count {
                    free.push(self.read_value()?);
                }
                let is_cont = self.read8()? != 0;
                Ok(Value::from(Closure::new(
                    self.ctx, code_block, &free, is_cont,
                )))
            }

            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Unsupported tag for FASL deserialization",
            )),
        }
    }

    fn read_closure_code_block(&mut self) -> io::Result<Gc<'gc, CodeBlock<'gc>>> {
        let tag = self.read8()?;
        match tag {
            FASL_TAG_GRAPH_DEF => {
                let index = self.read32()?;
                let inner_tag = self.read8()?;
                let value = match inner_tag {
                    FASL_TAG_CODE_BLOCK | FASL_TAG_UNLINKED_CODEBLOCK => {
                        let spec = self.read_code_block_spec()?;
                        Value::from(self.link_code_block_from_spec(Some(index), spec)?)
                    }
                    _ => self.read_tagged_value(inner_tag)?,
                };
                let graph = self.graph_stack.last_mut().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL graph definition outside graph context",
                    )
                })?;
                graph.define(index, value)?;
                self.resolve_pending_code_entry_relocations(index)?;
                self.resolve_pending_code_entry_slot_fills(index)?;
                self.resolve_pending_data_slot_fills(index)?;
                value.try_as::<CodeBlock>().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL closure code is not a code block",
                    )
                })
            }
            FASL_TAG_CODE_BLOCK | FASL_TAG_UNLINKED_CODEBLOCK => {
                let spec = self.read_code_block_spec()?;
                self.link_code_block_from_spec(None, spec)
            }
            _ => {
                let value = self.read_tagged_value(tag)?;
                value.try_as::<CodeBlock>().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL closure code is not a code block",
                    )
                })
            }
        }
    }

    fn read_code_block_spec(&mut self) -> io::Result<ReadCodeBlockSpec<'gc>> {
        let code_len = self.read32()? as usize;
        let mut bytes = vec![0; code_len];
        self.reader.read_exact(&mut bytes)?;
        let entry_offset = self.read32()?;
        if bytes.is_empty() || entry_offset as usize >= bytes.len() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL code block entry offset out of bounds",
            ));
        }
        let arity = self.read32()? as i32;
        let is_cont = self.read8()? != 0;
        let metadata = self.read_value()?;
        let relocation_count = self.read32()?;
        let mut relocations = Vec::with_capacity(relocation_count as usize);
        for _ in 0..relocation_count {
            relocations.push(reloc::Relocation::decode(&mut self.reader)?);
        }
        Ok(ReadCodeBlockSpec {
            bytes,
            entry_offset,
            arity,
            is_cont,
            metadata,
            relocations,
        })
    }

    fn read_relocatable_code_block(&mut self) -> io::Result<Gc<'gc, RelocatableCodeBlock<'gc>>> {
        let spec = self.read_code_block_spec()?;
        let data_slots = self.collect_code_block_data_slots(None, &spec.relocations)?;
        let encoded = reloc::encode_unlinked_relocations(&spec.relocations)?;
        let unlinked = RelocatableCodeBlock::new(
            self.ctx,
            &spec.bytes,
            spec.entry_offset,
            &encoded,
            &data_slots.value_bitmap,
        );
        self.keep_value(Value::from(unlinked));
        Ok(unlinked)
    }

    fn link_code_block_from_spec(
        &mut self,
        source_index: Option<u32>,
        spec: ReadCodeBlockSpec<'gc>,
    ) -> io::Result<Gc<'gc, CodeBlock<'gc>>> {
        let data_slots = self.collect_code_block_data_slots(source_index, &spec.relocations)?;
        let mut loaded = runtime_code_memory()
            .lock()
            .unwrap()
            .allocate_copy_with_data_slots(&spec.bytes, data_slots.slots.len())?;
        let result = (|| {
            self.initialize_loaded_data_slots(&loaded, &data_slots);
            self.add_pending_data_slot_fills(&loaded, &data_slots)?;
            self.add_pending_code_entry_slot_fills(&loaded, &data_slots)?;
            let encoded = reloc::encode_unlinked_relocations(&spec.relocations)?;
            let unlinked = RelocatableCodeBlock::new(
                self.ctx,
                &spec.bytes,
                spec.entry_offset,
                &encoded,
                &data_slots.value_bitmap,
            );
            self.keep_value(Value::from(unlinked));
            let code_block = CodeBlock::new_loaded_with_data(
                self.ctx,
                LoadedCodeBlockInit {
                    entrypoint: loaded.entrypoint + spec.entry_offset as usize,
                    arity: CodeArity::new(spec.arity),
                    is_continuation: spec.is_cont,
                    metadata: spec.metadata,
                    unlinked,
                    span: loaded.take_span().ok_or_else(|| {
                        io::Error::other("loaded code span has already been moved")
                    })?,
                    loaded_data_base: loaded.data_rw_base,
                    loaded_data_slot_count: loaded.data_len as u32,
                },
            );
            self.keep_value(Value::from(code_block));
            self.register_shared_cache_slots(source_index, code_block, &loaded, &data_slots)?;
            if let Err(err) = self.apply_code_block_relocations(
                source_index,
                code_block,
                &loaded,
                &spec.bytes,
                &spec.relocations,
                &data_slots,
            ) {
                // SAFETY: No concurrent access to the span; we own the code block
                if let Some(span) = unsafe { code_block.take_span_for_finalization() } {
                    let _ = runtime_code_memory().lock().unwrap().release_span(span);
                }
                return Err(err);
            }
            Ok(code_block)
        })();
        match result {
            Ok(code_block) => Ok(code_block),
            Err(err) => {
                if let Some(span) = loaded.take_span() {
                    let _ = runtime_code_memory().lock().unwrap().release_span(span);
                }
                Err(err)
            }
        }
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
        if version != FASL_VERSION {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "unsupported FASL version {version} (expected {})",
                    FASL_VERSION
                ),
            ));
        }
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

    pub fn new(ctx: Context<'gc>, reader: R) -> Self {
        let lites = HashTable::new(*ctx, HashTableType::Eq, 32, 0.75);
        let roots = Global::new(FaslReaderRoots {
            values: Monitor::new(vec![Value::from(lites)]),
        });
        Self {
            ctx,
            reader: BufReader::new(reader),
            lites,
            roots,
            reference_map: HashMap::new(),
            graph_stack: Vec::new(),
            shared_cache_slots: Vec::new(),
            pending_code_entry_relocations: Vec::new(),
            pending_data_slot_fills: Vec::new(),
            pending_code_entry_slot_fills: Vec::new(),
        }
    }

    fn keep_value(&self, value: Value<'gc>) {
        self.roots.fetch(*self.ctx).values.lock().push(value);
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
        let mut reader = FaslReader::new(self.ctx, Cursor::new(payload));
        reader.lites = self.lites;
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

    fn read_graph_definition(&mut self, index: u32) -> io::Result<Value<'gc>> {
        let tag = self.read8()?;
        if tag == FASL_TAG_DLIST {
            return self.read_graph_defined_dlist(index);
        }

        if tag != FASL_TAG_VECTOR {
            let value = if tag == FASL_TAG_CODE_BLOCK || tag == FASL_TAG_UNLINKED_CODEBLOCK {
                let spec = self.read_code_block_spec()?;
                Value::from(self.link_code_block_from_spec(Some(index), spec)?)
            } else {
                self.read_tagged_value(tag)?
            };
            let graph = self.graph_stack.last_mut().ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL graph definition outside graph context",
                )
            })?;
            graph.define(index, value)?;
            self.resolve_pending_code_entry_relocations(index)?;
            self.resolve_pending_code_entry_slot_fills(index)?;
            self.resolve_pending_data_slot_fills(index)?;
            return Ok(value);
        }

        let count = self.read32()? as usize;
        let vector = Vector::new::<false>(*self.ctx, count, Value::undefined());
        {
            let graph = self.graph_stack.last_mut().ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL graph definition outside graph context",
                )
            })?;
            graph.define(index, Value::new(vector))?;
        }
        self.resolve_pending_code_entry_relocations(index)?;
        self.resolve_pending_code_entry_slot_fills(index)?;
        self.resolve_pending_data_slot_fills(index)?;

        for i in 0..count {
            let value = self.read_value()?;
            Gc::write(*self.ctx, vector)[i].unlock().set(value);
        }
        Ok(Value::new(vector))
    }

    fn read_graph_defined_dlist(&mut self, index: u32) -> io::Result<Value<'gc>> {
        let count = self.read32()? as usize;
        if count == 0 {
            let value = self.read_value()?;
            let graph = self.graph_stack.last_mut().ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL graph definition outside graph context",
                )
            })?;
            graph.define(index, value)?;
            self.resolve_pending_code_entry_relocations(index)?;
            self.resolve_pending_code_entry_slot_fills(index)?;
            self.resolve_pending_data_slot_fills(index)?;
            return Ok(value);
        }

        let head = Pair::new(self.ctx, Value::undefined(), Value::undefined());
        {
            let graph = self.graph_stack.last_mut().ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL graph definition outside graph context",
                )
            })?;
            graph.define(index, Value::new(head))?;
        }
        self.resolve_pending_code_entry_relocations(index)?;
        self.resolve_pending_code_entry_slot_fills(index)?;
        self.resolve_pending_data_slot_fills(index)?;

        let mut current = head;
        for i in 0..count {
            let car = self.read_value()?;
            current.set_car(self.ctx, car);
            if i + 1 == count {
                let cdr = self.read_value()?;
                current.set_cdr(self.ctx, cdr);
            } else {
                let next = Pair::new(self.ctx, Value::undefined(), Value::undefined());
                current.set_cdr(self.ctx, Value::new(next));
                current = next;
            }
        }
        Ok(Value::new(head))
    }

    fn apply_code_block_relocations(
        &mut self,
        source_index: Option<u32>,
        code_block: Gc<'gc, CodeBlock<'gc>>,
        loaded: &CodeAllocation,
        code_bytes: &[u8],
        relocations: &[reloc::Relocation],
        data_slots: &CodeDataSlots<'gc>,
    ) -> io::Result<()> {
        let mut patches = Vec::with_capacity(relocations.len());
        for relocation in relocations {
            if let Some(patch) = self.resolve_code_block_relocation_patch(
                source_index,
                loaded,
                code_bytes,
                relocation,
                data_slots,
            )? {
                patches.push(patch);
            }
        }
        let patch_refs = patches
            .iter()
            .map(|(offset, bytes)| (*offset, bytes.as_slice()))
            .collect::<Vec<_>>();
        code_block.with_live_span(|span| {
            runtime_code_memory()
                .lock()
                .unwrap()
                .patch_many_raw(span, &patch_refs)
        })
    }

    fn resolve_code_block_relocation_patch(
        &mut self,
        source_index: Option<u32>,
        loaded: &CodeAllocation,
        code_bytes: &[u8],
        relocation: &reloc::Relocation,
        data_slots: &CodeDataSlots<'gc>,
    ) -> io::Result<Option<(usize, Vec<u8>)>> {
        let offset = usize::try_from(relocation.offset).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL code block relocation offset is too large",
            )
        })?;
        let target_address = match (&relocation.kind, relocation.target) {
            (reloc::RelocKind::Asmkit(_), reloc::RelocTarget::RuntimeSymbol(symbol_id)) => {
                if let Some(runtime_data) = RuntimeData::from_id(symbol_id) {
                    runtime_data.address().as_usize()
                } else {
                    RuntimeThunk::from_id(symbol_id)
                        .ok_or_else(|| {
                            io::Error::new(
                                io::ErrorKind::InvalidData,
                                "unknown FASL runtime symbol",
                            )
                        })?
                        .address()
                        .as_usize()
                }
            }
            (reloc::RelocKind::Asmkit(_), reloc::RelocTarget::SideMetadata(kind)) => {
                patch::side_metadata_address(kind)
            }
            (
                reloc::RelocKind::Asmkit(_),
                reloc::RelocTarget::Entry(index) | reloc::RelocTarget::Object(index),
            ) => match self.graph_code_block_entrypoint_if_defined(index)? {
                Some(entrypoint) => entrypoint,
                None => {
                    let source_index = source_index.ok_or_else(|| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            "FASL pending code entry relocation outside graph definition",
                        )
                    })?;
                    self.add_pending_code_entry_relocation(PendingCodeEntryRelocation {
                        source_index,
                        base: loaded.entrypoint,
                        offset,
                        target_index: index,
                        kind: relocation.kind.clone(),
                        addend: relocation.addend,
                        original_bytes: patch::relocation_original_bytes(
                            code_bytes,
                            offset,
                            &relocation.kind,
                        )?
                        .to_vec(),
                    })?;
                    return Ok(None);
                }
            },
            (reloc::RelocKind::Cranelift(_), reloc::RelocTarget::RuntimeSymbol(symbol_id)) => {
                if let Some(runtime_data) = RuntimeData::from_id(symbol_id) {
                    runtime_data.address().as_usize()
                } else {
                    RuntimeThunk::from_id(symbol_id)
                        .ok_or_else(|| {
                            io::Error::new(
                                io::ErrorKind::InvalidData,
                                "unknown FASL runtime symbol",
                            )
                        })?
                        .address()
                        .as_usize()
                }
            }
            (reloc::RelocKind::Cranelift(_), reloc::RelocTarget::SideMetadata(kind)) => {
                patch::side_metadata_address(kind)
            }
            (
                reloc::RelocKind::Cranelift(_),
                reloc::RelocTarget::Object(_) | reloc::RelocTarget::CacheCell(_),
            ) => data_slots.slot_rw_address(loaded, relocation.target)?,
            (reloc::RelocKind::CraneliftDataSlot(_), _) => {
                data_slots.slot_rw_address(loaded, relocation.target)?
            }
            (reloc::RelocKind::Cranelift(_), reloc::RelocTarget::Entry(index)) => {
                match self.graph_code_block_entrypoint_if_defined(index)? {
                    Some(entrypoint) => entrypoint,
                    None => {
                        let source_index = source_index.ok_or_else(|| {
                            io::Error::new(
                                io::ErrorKind::InvalidData,
                                "FASL pending code entry relocation outside graph definition",
                            )
                        })?;
                        self.add_pending_code_entry_relocation(PendingCodeEntryRelocation {
                            source_index,
                            base: loaded.entrypoint,
                            offset,
                            target_index: index,
                            kind: relocation.kind.clone(),
                            addend: relocation.addend,
                            original_bytes: patch::relocation_original_bytes(
                                code_bytes,
                                offset,
                                &relocation.kind,
                            )?
                            .to_vec(),
                        })?;
                        return Ok(None);
                    }
                }
            }
            (
                reloc::RelocKind::RuntimeData | reloc::RelocKind::AbsWord,
                reloc::RelocTarget::RuntimeSymbol(symbol_id),
            ) => {
                let runtime_data = RuntimeData::from_id(symbol_id).ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "unknown FASL runtime data symbol",
                    )
                })?;
                runtime_data.address().as_usize()
            }
            (reloc::RelocKind::RuntimeThunk, reloc::RelocTarget::RuntimeSymbol(symbol_id)) => {
                let runtime_thunk = RuntimeThunk::from_id(symbol_id).ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "unknown FASL runtime thunk symbol",
                    )
                })?;
                runtime_thunk.address().as_usize()
            }
            (reloc::RelocKind::SideMetadata, reloc::RelocTarget::SideMetadata(kind)) => {
                patch::side_metadata_address(kind)
            }
            (reloc::RelocKind::DataSlotAddress, target) => {
                data_slots.slot_rw_address(loaded, target)?
            }
            (reloc::RelocKind::CacheCell, target) => data_slots.slot_rw_address(loaded, target)?,
            (
                reloc::RelocKind::CodeEntry,
                reloc::RelocTarget::Entry(index) | reloc::RelocTarget::Object(index),
            ) => match self.graph_code_block_entrypoint_if_defined(index)? {
                Some(entrypoint) => entrypoint,
                None => {
                    let source_index = source_index.ok_or_else(|| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            "FASL pending code entry relocation outside graph definition",
                        )
                    })?;
                    self.add_pending_code_entry_relocation(PendingCodeEntryRelocation {
                        source_index,
                        base: loaded.entrypoint,
                        offset,
                        target_index: index,
                        kind: relocation.kind.clone(),
                        addend: relocation.addend,
                        original_bytes: patch::relocation_original_bytes(
                            code_bytes,
                            offset,
                            &relocation.kind,
                        )?
                        .to_vec(),
                    })?;
                    return Ok(None);
                }
            },
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "unsupported FASL code block relocation",
                ));
            }
        };
        patch::relocation_patch_bytes(
            loaded.entrypoint,
            offset,
            &relocation.kind,
            target_address,
            relocation.addend,
            patch::relocation_original_bytes(code_bytes, offset, &relocation.kind)?,
        )
        .map(|bytes| Some((offset, bytes)))
    }

    fn collect_code_block_data_slots(
        &mut self,
        source_index: Option<u32>,
        relocations: &[reloc::Relocation],
    ) -> io::Result<CodeDataSlots<'gc>> {
        let mut slots = CodeDataSlots::default();
        for relocation in relocations {
            if let Some(index) = code_entry_keepalive_index(relocation) {
                let target = reloc::RelocTarget::Object(index);
                match slots.indices.entry(target) {
                    Entry::Occupied(_) => {}
                    Entry::Vacant(_) => {
                        if let Some(value) = self.graph_value_optional(index)? {
                            slots.push_value(target, value)?;
                        } else {
                            slots.push_pending_value(target, index)?;
                        }
                    }
                }
            }

            if matches!(relocation.target, reloc::RelocTarget::CacheCell(_)) {
                let reloc::RelocTarget::CacheCell(index) = relocation.target else {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL cache-cell relocation target is not an object",
                    ));
                };
                if let Some(shared) = self.shared_cache_slot(index)? {
                    slots.push_keepalive_value(shared.owner);
                    slots
                        .external_addresses
                        .insert(relocation.target, shared.address);
                    continue;
                }
                let _ = source_index;
            }

            if !matches!(relocation.kind, reloc::RelocKind::DataSlotAddress)
                && !matches!(relocation.kind, reloc::RelocKind::CacheCell)
                && !matches!(relocation.kind, reloc::RelocKind::CraneliftDataSlot(_))
                && !matches!(
                    (&relocation.kind, relocation.target),
                    (
                        reloc::RelocKind::Cranelift(_),
                        reloc::RelocTarget::Object(_) | reloc::RelocTarget::CacheCell(_)
                    )
                )
            {
                continue;
            }
            let target = relocation.target;
            let is_local_cache_cell = matches!(target, reloc::RelocTarget::CacheCell(_));
            if slots.indices.contains_key(&target) {
                continue;
            }
            match target {
                reloc::RelocTarget::Object(index) | reloc::RelocTarget::CacheCell(index) => {
                    if let Some(value) = self.graph_value_optional(index)? {
                        slots.push_value(target, value)?;
                    } else {
                        slots.push_pending_value(target, index)?;
                    }
                }
                reloc::RelocTarget::Entry(index) => {
                    if let Some(entrypoint) = self.graph_code_block_entrypoint_if_defined(index)? {
                        slots.push_raw(target, entrypoint)?;
                    } else {
                        slots.push_pending_entry(target, index)?;
                    }
                }
                reloc::RelocTarget::SideMetadata(kind) => {
                    slots.push_raw(target, patch::side_metadata_address(kind))?;
                }
                reloc::RelocTarget::RuntimeSymbol(_) => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL runtime-symbol data slots are not supported",
                    ));
                }
            }
            if is_local_cache_cell {
                let reloc::RelocTarget::CacheCell(index) = target else {
                    unreachable!("cache-cell relocation target was validated above");
                };
                slots.local_cache_indices.push(index);
            }
        }
        Ok(slots)
    }

    fn shared_cache_slot(&self, index: u32) -> io::Result<Option<SharedCacheSlot<'gc>>> {
        let Some(shared) = self.shared_cache_slots.last() else {
            return Ok(None);
        };
        Ok(shared.get(&index).copied())
    }

    fn register_shared_cache_slots(
        &mut self,
        source_index: Option<u32>,
        code_block: Gc<'gc, CodeBlock<'gc>>,
        loaded: &CodeAllocation,
        data_slots: &CodeDataSlots<'gc>,
    ) -> io::Result<()> {
        let Some(_source_index) = source_index else {
            return Ok(());
        };
        let Some(shared) = self.shared_cache_slots.last_mut() else {
            return Ok(());
        };
        for graph_index in &data_slots.local_cache_indices {
            let slot_index = data_slots.slot_index(reloc::RelocTarget::CacheCell(*graph_index))?;
            shared.entry(*graph_index).or_insert(SharedCacheSlot {
                address: loaded.data_rw_base + slot_index * std::mem::size_of::<Value<'gc>>(),
                owner: Value::from(code_block),
            });
        }
        Ok(())
    }

    fn initialize_loaded_data_slots(
        &self,
        loaded: &CodeAllocation,
        data_slots: &CodeDataSlots<'gc>,
    ) {
        if data_slots.slots.is_empty() {
            return;
        }
        debug_assert_eq!(loaded.data_len, data_slots.slots.len());
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let slots = loaded.data_rw_base.as_usize() as *mut usize;
            for (index, slot) in data_slots.slots.iter().copied().enumerate() {
                slots.add(index).write(slot.as_word());
            }
        }
    }

    fn add_pending_data_slot_fills(
        &mut self,
        loaded: &CodeAllocation,
        data_slots: &CodeDataSlots<'gc>,
    ) -> io::Result<()> {
        if data_slots.pending_value_indices.is_empty() {
            return Ok(());
        }
        let pending = self.pending_data_slot_fills.last_mut().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL data slot relocation outside graph context",
            )
        })?;
        for (target, graph_index) in &data_slots.pending_value_indices {
            let slot_index = data_slots.slot_index(*target)?;
            pending.push(PendingDataSlotFill {
                target_index: *graph_index,
                slot_address: loaded.data_rw_base + slot_index * std::mem::size_of::<Value<'gc>>(),
            });
        }
        Ok(())
    }

    fn add_pending_code_entry_slot_fills(
        &mut self,
        loaded: &CodeAllocation,
        data_slots: &CodeDataSlots<'gc>,
    ) -> io::Result<()> {
        if data_slots.pending_entry_indices.is_empty() {
            return Ok(());
        }
        let pending = self
            .pending_code_entry_slot_fills
            .last_mut()
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL code entry data slot relocation outside graph context",
                )
            })?;
        for code_index in &data_slots.pending_entry_indices {
            let target = reloc::RelocTarget::Entry(*code_index);
            let slot_index = data_slots.slot_index(target)?;
            pending.push(PendingCodeEntrySlotFill {
                target_index: *code_index,
                slot_address: loaded.data_rw_base + slot_index * std::mem::size_of::<usize>(),
            });
        }
        Ok(())
    }

    fn graph_value(&self, index: u32) -> io::Result<Value<'gc>> {
        let graph = self.graph_stack.last().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL object data slot relocation outside graph context",
            )
        })?;
        graph.get(index)
    }

    fn graph_value_optional(&self, index: u32) -> io::Result<Option<Value<'gc>>> {
        let graph = self.graph_stack.last().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL object relocation outside graph context",
            )
        })?;
        graph.get_optional(index)
    }

    fn graph_code_block_entrypoint_if_defined(&self, index: u32) -> io::Result<Option<usize>> {
        let Some(value) = self.graph_value_optional(index)? else {
            return Ok(None);
        };
        let code_block = value.try_as::<CodeBlock>().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL code entry relocation target is not a code block",
            )
        })?;
        Ok(Some(code_block.entrypoint.as_usize()))
    }

    fn add_pending_code_entry_relocation(
        &mut self,
        relocation: PendingCodeEntryRelocation,
    ) -> io::Result<()> {
        let pending = self
            .pending_code_entry_relocations
            .last_mut()
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL code entry relocation outside graph context",
                )
            })?;
        pending.push(relocation);
        Ok(())
    }

    fn resolve_pending_code_entry_relocations(&mut self, index: u32) -> io::Result<()> {
        let Some(pending) = self.pending_code_entry_relocations.last_mut() else {
            return Ok(());
        };
        let mut remaining = Vec::with_capacity(pending.len());
        let mut resolved = Vec::new();
        for relocation in pending.drain(..) {
            if relocation.target_index == index {
                resolved.push(relocation);
            } else {
                remaining.push(relocation);
            }
        }
        *pending = remaining;
        if resolved.is_empty() {
            return Ok(());
        }
        let target = self
            .graph_code_block_entrypoint_if_defined(index)?
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "unresolved FASL code entry relocation",
                )
            })?;
        let graph = self.graph_stack.last().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL code entry relocation outside graph context",
            )
        })?;
        let mut patches = Vec::with_capacity(resolved.len());
        for relocation in resolved {
            let source = graph.get(relocation.source_index)?;
            let code_block = source.try_as::<CodeBlock>().ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL code entry relocation source is not a code block",
                )
            })?;
            let bytes = patch::relocation_patch_bytes(
                relocation.base,
                relocation.offset,
                &relocation.kind,
                target,
                relocation.addend,
                &relocation.original_bytes,
            )?;
            patches.push((code_block, relocation.offset, bytes));
        }
        let mut memory = runtime_code_memory().lock().unwrap();
        for (code_block, offset, bytes) in &mut patches {
            code_block.with_live_span(|span| memory.patch_raw(span, *offset, bytes))?;
        }
        Ok(())
    }

    fn resolve_pending_code_entry_slot_fills(&mut self, index: u32) -> io::Result<()> {
        let Some(pending) = self.pending_code_entry_slot_fills.last_mut() else {
            return Ok(());
        };
        let mut remaining = Vec::with_capacity(pending.len());
        let mut resolved = Vec::new();
        for fill in pending.drain(..) {
            if fill.target_index == index {
                resolved.push(fill);
            } else {
                remaining.push(fill);
            }
        }
        *pending = remaining;
        if resolved.is_empty() {
            return Ok(());
        }
        let target = self
            .graph_code_block_entrypoint_if_defined(index)?
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "unresolved FASL code entry data slot fill",
                )
            })?;
        for fill in resolved {
            // SAFETY: Preconditions verified by the surrounding code
            unsafe {
                (fill.slot_address.as_usize() as *mut usize).write(target);
            }
        }
        Ok(())
    }

    fn resolve_pending_data_slot_fills(&mut self, index: u32) -> io::Result<()> {
        let Some(pending) = self.pending_data_slot_fills.last_mut() else {
            return Ok(());
        };
        let mut remaining = Vec::with_capacity(pending.len());
        let mut resolved = Vec::new();
        for fill in pending.drain(..) {
            if fill.target_index == index {
                resolved.push(fill);
            } else {
                remaining.push(fill);
            }
        }
        *pending = remaining;
        if resolved.is_empty() {
            return Ok(());
        }
        let value = self.graph_value(index)?;
        for fill in resolved {
            // SAFETY: Preconditions verified by the surrounding code
            unsafe {
                (fill.slot_address.as_usize() as *mut Value<'gc>).write(value);
            }
        }
        Ok(())
    }
}

fn code_entry_keepalive_index(relocation: &reloc::Relocation) -> Option<u32> {
    match (&relocation.kind, relocation.target) {
        (
            reloc::RelocKind::Asmkit(_) | reloc::RelocKind::CodeEntry,
            reloc::RelocTarget::Entry(index) | reloc::RelocTarget::Object(index),
        ) => Some(index),
        (
            reloc::RelocKind::Cranelift(_)
            | reloc::RelocKind::DataSlotAddress
            | reloc::RelocKind::CraneliftDataSlot(_),
            reloc::RelocTarget::Entry(index),
        ) => Some(index),
        _ => None,
    }
}

#[derive(Clone, Copy)]
enum CodeDataSlot<'gc> {
    Value(Value<'gc>),
    Raw(usize),
}

impl CodeDataSlot<'_> {
    fn as_word(self) -> usize {
        match self {
            Self::Value(value) => value.bits() as usize,
            Self::Raw(word) => word,
        }
    }
}

#[derive(Default)]
struct CodeDataSlots<'gc> {
    indices: HashMap<reloc::RelocTarget, usize>,
    external_addresses: HashMap<reloc::RelocTarget, Address>,
    slots: Vec<CodeDataSlot<'gc>>,
    value_bitmap: Vec<usize>,
    pending_value_indices: Vec<(reloc::RelocTarget, u32)>,
    pending_entry_indices: Vec<u32>,
    local_cache_indices: Vec<u32>,
}

impl<'gc> CodeDataSlots<'gc> {
    fn push_value(&mut self, target: reloc::RelocTarget, value: Value<'gc>) -> io::Result<()> {
        let slot_index = self.slots.len();
        let word_index = slot_index / usize::BITS as usize;
        if self.value_bitmap.len() <= word_index {
            self.value_bitmap.resize(word_index + 1, 0);
        }
        self.indices.insert(target, slot_index);
        self.slots.push(CodeDataSlot::Value(value));
        self.value_bitmap[word_index] |= 1usize << (slot_index % usize::BITS as usize);
        Ok(())
    }

    fn push_keepalive_value(&mut self, value: Value<'gc>) {
        let slot_index = self.slots.len();
        let word_index = slot_index / usize::BITS as usize;
        if self.value_bitmap.len() <= word_index {
            self.value_bitmap.resize(word_index + 1, 0);
        }
        self.slots.push(CodeDataSlot::Value(value));
        self.value_bitmap[word_index] |= 1usize << (slot_index % usize::BITS as usize);
    }

    fn push_raw(&mut self, target: reloc::RelocTarget, word: usize) -> io::Result<()> {
        let slot_index = self.slots.len();
        self.indices.insert(target, slot_index);
        self.slots.push(CodeDataSlot::Raw(word));
        Ok(())
    }

    fn push_pending_value(
        &mut self,
        target: reloc::RelocTarget,
        graph_index: u32,
    ) -> io::Result<()> {
        self.push_value(target, Value::undefined())?;
        self.pending_value_indices.push((target, graph_index));
        Ok(())
    }

    fn push_pending_entry(
        &mut self,
        target: reloc::RelocTarget,
        code_index: u32,
    ) -> io::Result<()> {
        self.push_raw(target, 0)?;
        self.pending_entry_indices.push(code_index);
        Ok(())
    }

    fn slot_index(&self, target: reloc::RelocTarget) -> io::Result<usize> {
        self.indices.get(&target).copied().ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL data slot relocation references unknown slot",
            )
        })
    }

    fn slot_rw_address(
        &self,
        loaded: &CodeAllocation,
        target: reloc::RelocTarget,
    ) -> io::Result<usize> {
        if let Some(address) = self.external_addresses.get(&target) {
            return Ok(address.as_usize());
        }
        let slot_index = self.slot_index(target)?;
        Ok(loaded.data_rw_base.as_usize() + slot_index * std::mem::size_of::<Value<'gc>>())
    }
}
