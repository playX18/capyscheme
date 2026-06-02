use std::{
    collections::{HashMap, hash_map::Entry},
    io::{self, BufReader, Cursor, Read},
};

use flate2::read::GzDecoder;
use mmtk::util::metadata::side_metadata::{
    global_side_metadata_vm_base_address, vo_bit_side_metadata_addr,
};

use asmkit::core::buffer::Reloc as AsmkitReloc;
use cranelift_codegen::binemit::Reloc as CraneliftReloc;

use crate::rsgc::{Gc, Global, Trace, Visitor, mmtk::util::Address, sync::monitor::Monitor};

use crate::runtime::{
    Context,
    code_memory::{CodeAllocation, runtime_code_memory},
    symbols::{RuntimeData, RuntimeThunk},
    value::{
        BigInt, ByteVector, Closure, CodeArity, CodeBlock, Complex, HashTable, HashTableType, Pair,
        Rational, Str, Symbol, Tuple, UnlinkedCodeBlock, UnlinkedRelocation, Value, Vector,
    },
    vm::syntax::Syntax,
};

use super::{
    FASL_MAGIC, FASL_RELOC_ABS_WORD, FASL_RELOC_ASMKIT, FASL_RELOC_CACHE_CELL,
    FASL_RELOC_CODE_ENTRY, FASL_RELOC_CRANELIFT, FASL_RELOC_CRANELIFT_DATA_SLOT,
    FASL_RELOC_DATA_SLOT, FASL_RELOC_RUNTIME_DATA, FASL_RELOC_RUNTIME_THUNK,
    FASL_RELOC_SIDE_METADATA, FASL_SITUATION_VISIT_REVISIT, FASL_TAG_BEGIN, FASL_TAG_BIGINT,
    FASL_TAG_BVECTOR, FASL_TAG_CHAR, FASL_TAG_CLOSURE, FASL_TAG_CODE_BLOCK, FASL_TAG_COMPLEX,
    FASL_TAG_DLIST, FASL_TAG_ENTRY, FASL_TAG_F, FASL_TAG_FIXNUM, FASL_TAG_FLONUM, FASL_TAG_GRAPH,
    FASL_TAG_GRAPH_DEF, FASL_TAG_GRAPH_REF, FASL_TAG_GROUP, FASL_TAG_GZIP, FASL_TAG_IMMEDIATE,
    FASL_TAG_KEYWORD, FASL_TAG_LOOKUP, FASL_TAG_LZ4, FASL_TAG_NIL, FASL_TAG_PLIST,
    FASL_TAG_RATIONAL, FASL_TAG_REF, FASL_TAG_REF_INIT, FASL_TAG_STR, FASL_TAG_SYMBOL,
    FASL_TAG_SYNTAX, FASL_TAG_T, FASL_TAG_TUPLE, FASL_TAG_UNCOMPRESSED, FASL_TAG_UNINTERNED_SYMBOL,
    FASL_TAG_UNLINKED_CODEBLOCK, FASL_TAG_VECTOR, FASL_VERSION, graph, reloc,
};

fn read_u8_from(input: &mut impl Read) -> io::Result<u8> {
    let mut buf = [0; 1];
    input.read_exact(&mut buf)?;
    Ok(buf[0])
}

fn read_u32_from(input: &mut impl Read) -> io::Result<u32> {
    let mut buf = [0; 4];
    input.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf))
}

pub struct FASLReader<'gc, R: io::Read> {
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

unsafe impl<'gc> Trace for FaslReaderRoots<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        for value in self.values.get_mut().iter_mut() {
            visitor.trace(value);
        }
    }

    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}
}

unsafe impl Send for FaslReaderRoots<'_> {}
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
    kind: reloc::FaslRelocKind,
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
    relocations: Vec<reloc::FaslRelocation>,
}

const UNLINKED_TARGET_OBJECT: u8 = 0;
const UNLINKED_TARGET_ENTRY: u8 = 1;
const UNLINKED_TARGET_RUNTIME_SYMBOL: u8 = 2;
const UNLINKED_TARGET_SIDE_METADATA: u8 = 3;
const UNLINKED_TARGET_CACHE_CELL: u8 = 4;

pub(crate) fn encode_unlinked_relocations(
    relocations: &[reloc::FaslRelocation],
) -> io::Result<Vec<UnlinkedRelocation>> {
    relocations
        .iter()
        .map(|relocation| {
            let (kind_tag, aux_tag) = match &relocation.kind {
                reloc::FaslRelocKind::Asmkit(reloc) => {
                    (FASL_RELOC_ASMKIT, reloc::asmkit_reloc_to_tag(*reloc))
                }
                reloc::FaslRelocKind::Cranelift(reloc) => {
                    (FASL_RELOC_CRANELIFT, reloc::cranelift_reloc_to_tag(*reloc))
                }
                reloc::FaslRelocKind::CraneliftDataSlot(reloc) => (
                    FASL_RELOC_CRANELIFT_DATA_SLOT,
                    reloc::cranelift_reloc_to_tag(*reloc),
                ),
                reloc::FaslRelocKind::AbsWord => (FASL_RELOC_ABS_WORD, 0),
                reloc::FaslRelocKind::CodeEntry => (FASL_RELOC_CODE_ENTRY, 0),
                reloc::FaslRelocKind::DataSlotAddress => (FASL_RELOC_DATA_SLOT, 0),
                reloc::FaslRelocKind::RuntimeThunk => (FASL_RELOC_RUNTIME_THUNK, 0),
                reloc::FaslRelocKind::RuntimeData => (FASL_RELOC_RUNTIME_DATA, 0),
                reloc::FaslRelocKind::SideMetadata => (FASL_RELOC_SIDE_METADATA, 0),
                reloc::FaslRelocKind::CacheCell => (FASL_RELOC_CACHE_CELL, 0),
            };
            let (target_tag, target_payload) = match relocation.target {
                reloc::FaslRelocTarget::Object(index) => (UNLINKED_TARGET_OBJECT, index),
                reloc::FaslRelocTarget::Entry(index) => (UNLINKED_TARGET_ENTRY, index),
                reloc::FaslRelocTarget::CacheCell(index) => (UNLINKED_TARGET_CACHE_CELL, index),
                reloc::FaslRelocTarget::RuntimeSymbol(symbol) => {
                    (UNLINKED_TARGET_RUNTIME_SYMBOL, symbol)
                }
                reloc::FaslRelocTarget::SideMetadata(kind) => {
                    (UNLINKED_TARGET_SIDE_METADATA, kind.to_tag() as u32)
                }
            };
            Ok(UnlinkedRelocation {
                offset: relocation.offset,
                kind_tag,
                target_tag,
                target_payload,
                addend: relocation.addend,
                aux_tag,
            })
        })
        .collect()
}

#[cfg_attr(not(test), allow(dead_code))]
pub(crate) fn decode_unlinked_relocations(
    relocations: &[UnlinkedRelocation],
) -> io::Result<Vec<reloc::FaslRelocation>> {
    relocations
        .iter()
        .map(|relocation| {
            let kind = match relocation.kind_tag {
                FASL_RELOC_ASMKIT => {
                    reloc::FaslRelocKind::Asmkit(reloc::asmkit_reloc_from_tag(relocation.aux_tag)?)
                }
                FASL_RELOC_CRANELIFT => reloc::FaslRelocKind::Cranelift(
                    reloc::cranelift_reloc_from_tag(relocation.aux_tag)?,
                ),
                FASL_RELOC_CRANELIFT_DATA_SLOT => reloc::FaslRelocKind::CraneliftDataSlot(
                    reloc::cranelift_reloc_from_tag(relocation.aux_tag)?,
                ),
                FASL_RELOC_ABS_WORD => reloc::FaslRelocKind::AbsWord,
                FASL_RELOC_CODE_ENTRY => reloc::FaslRelocKind::CodeEntry,
                FASL_RELOC_DATA_SLOT => reloc::FaslRelocKind::DataSlotAddress,
                FASL_RELOC_RUNTIME_THUNK => reloc::FaslRelocKind::RuntimeThunk,
                FASL_RELOC_RUNTIME_DATA => reloc::FaslRelocKind::RuntimeData,
                FASL_RELOC_SIDE_METADATA => reloc::FaslRelocKind::SideMetadata,
                FASL_RELOC_CACHE_CELL => reloc::FaslRelocKind::CacheCell,
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "invalid unlinked relocation kind tag",
                    ));
                }
            };
            let target = match relocation.target_tag {
                UNLINKED_TARGET_OBJECT => reloc::FaslRelocTarget::Object(relocation.target_payload),
                UNLINKED_TARGET_ENTRY => reloc::FaslRelocTarget::Entry(relocation.target_payload),
                UNLINKED_TARGET_CACHE_CELL => {
                    reloc::FaslRelocTarget::CacheCell(relocation.target_payload)
                }
                UNLINKED_TARGET_RUNTIME_SYMBOL => {
                    reloc::FaslRelocTarget::RuntimeSymbol(relocation.target_payload)
                }
                UNLINKED_TARGET_SIDE_METADATA => {
                    reloc::FaslRelocTarget::SideMetadata(reloc::SideMetadataSlotKind::from_tag(
                        u8::try_from(relocation.target_payload).map_err(|_| {
                            io::Error::new(
                                io::ErrorKind::InvalidData,
                                "invalid unlinked side metadata tag",
                            )
                        })?,
                    )?)
                }
                _ => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "invalid unlinked relocation target tag",
                    ));
                }
            };
            Ok(reloc::FaslRelocation {
                offset: relocation.offset,
                kind,
                target,
                addend: relocation.addend,
            })
        })
        .collect()
}

impl<'gc, R: io::Read> FASLReader<'gc, R> {
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

                    let sym = Value::new(Symbol::from_str(self.ctx, str));

                    sym
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

            self.lites.put(self.ctx, Value::new(id as i32), key);
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
                let value = self.read32()? as u32;
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
                    return Ok(value);
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Unknown lookup ID",
                    ));
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

            _x @ FASL_TAG_GROUP => {
                let situation = self.read8()?;
                if situation != FASL_SITUATION_VISIT_REVISIT {
                    return Err(io::Error::new(
                        io::ErrorKind::Unsupported,
                        "FASL visit/revisit split groups are not supported yet",
                    ));
                }
                let group_size = self.read32()? as usize;
                let mut group = vec![0; group_size];
                self.reader.read_exact(&mut group)?;
                self.read_pcfasl_value(group)
            }

            _x @ FASL_TAG_GZIP => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL gzip wrapper outside group",
            )),

            _x @ FASL_TAG_LZ4 => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL LZ4 wrapper outside group",
            )),

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
                let unlinked = self.read_unlinked_code_block()?;
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

            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Unsupported tag for FASL deserialization",
                ));
            }
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
            relocations.push(reloc::FaslRelocation::decode(&mut self.reader)?);
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

    fn read_unlinked_code_block(&mut self) -> io::Result<Gc<'gc, UnlinkedCodeBlock<'gc>>> {
        let spec = self.read_code_block_spec()?;
        let data_slots = self.collect_code_block_data_slots(None, &spec.relocations)?;
        let encoded = encode_unlinked_relocations(&spec.relocations)?;
        let unlinked = UnlinkedCodeBlock::new(
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
            let encoded = encode_unlinked_relocations(&spec.relocations)?;
            let unlinked = UnlinkedCodeBlock::new(
                self.ctx,
                &spec.bytes,
                spec.entry_offset,
                &encoded,
                &data_slots.value_bitmap,
            );
            self.keep_value(Value::from(unlinked));
            let code_block = CodeBlock::new_loaded_with_data(
                self.ctx,
                loaded.entrypoint + spec.entry_offset as usize,
                CodeArity::new(spec.arity),
                spec.is_cont,
                spec.metadata,
                unlinked,
                loaded
                    .take_span()
                    .ok_or_else(|| io::Error::other("loaded code span has already been moved"))?,
                loaded.data_rw_base,
                loaded.data_len as u32,
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
        self.read_lites()?;
        let value = self.read_value()?;
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

    fn read_pcfasl_value(&self, group: Vec<u8>) -> io::Result<Value<'gc>> {
        let mut cursor = Cursor::new(group);
        let tag = read_u8_from(&mut cursor)?;
        let payload = match tag {
            FASL_TAG_UNCOMPRESSED => {
                let size = read_u32_from(&mut cursor)? as usize;
                let mut payload = vec![0; size];
                cursor.read_exact(&mut payload)?;
                payload
            }
            FASL_TAG_GZIP => {
                let uncompressed_size = read_u32_from(&mut cursor)? as usize;
                let compressed_size = read_u32_from(&mut cursor)? as usize;
                let mut compressed = vec![0; compressed_size];
                cursor.read_exact(&mut compressed)?;
                let mut decoder = GzDecoder::new(Cursor::new(compressed));
                let mut payload = Vec::with_capacity(uncompressed_size);
                decoder.read_to_end(&mut payload)?;
                if payload.len() != uncompressed_size {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL gzip payload size mismatch",
                    ));
                }
                payload
            }
            FASL_TAG_LZ4 => {
                return Err(io::Error::new(
                    io::ErrorKind::Unsupported,
                    "FASL LZ4 compression is not supported yet",
                ));
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "unknown FASL payload compression tag",
                ));
            }
        };

        if cursor.position() != cursor.get_ref().len() as u64 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL group has trailing bytes",
            ));
        }

        let mut reader = FASLReader::new(self.ctx, Cursor::new(payload));
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
        relocations: &[reloc::FaslRelocation],
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
        relocation: &reloc::FaslRelocation,
        data_slots: &CodeDataSlots<'gc>,
    ) -> io::Result<Option<(usize, Vec<u8>)>> {
        let offset = usize::try_from(relocation.offset).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL code block relocation offset is too large",
            )
        })?;
        let target_address = match (&relocation.kind, relocation.target) {
            (reloc::FaslRelocKind::Asmkit(_), reloc::FaslRelocTarget::RuntimeSymbol(symbol_id)) => {
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
            (reloc::FaslRelocKind::Asmkit(_), reloc::FaslRelocTarget::SideMetadata(kind)) => {
                side_metadata_address(kind)
            }
            (
                reloc::FaslRelocKind::Asmkit(_),
                reloc::FaslRelocTarget::Entry(index) | reloc::FaslRelocTarget::Object(index),
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
                        original_bytes: relocation_original_bytes(
                            code_bytes,
                            offset,
                            &relocation.kind,
                        )?
                        .to_vec(),
                    })?;
                    return Ok(None);
                }
            },
            (
                reloc::FaslRelocKind::Cranelift(_),
                reloc::FaslRelocTarget::RuntimeSymbol(symbol_id),
            ) => {
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
            (reloc::FaslRelocKind::Cranelift(_), reloc::FaslRelocTarget::SideMetadata(kind)) => {
                side_metadata_address(kind)
            }
            (
                reloc::FaslRelocKind::Cranelift(_),
                reloc::FaslRelocTarget::Object(_) | reloc::FaslRelocTarget::CacheCell(_),
            ) => data_slots.slot_rw_address(loaded, relocation.target)?,
            (reloc::FaslRelocKind::CraneliftDataSlot(_), _) => {
                data_slots.slot_rw_address(loaded, relocation.target)?
            }
            (reloc::FaslRelocKind::Cranelift(_), reloc::FaslRelocTarget::Entry(index)) => {
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
                            original_bytes: relocation_original_bytes(
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
                reloc::FaslRelocKind::RuntimeData | reloc::FaslRelocKind::AbsWord,
                reloc::FaslRelocTarget::RuntimeSymbol(symbol_id),
            ) => {
                let runtime_data = RuntimeData::from_id(symbol_id).ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "unknown FASL runtime data symbol",
                    )
                })?;
                runtime_data.address().as_usize()
            }
            (
                reloc::FaslRelocKind::RuntimeThunk,
                reloc::FaslRelocTarget::RuntimeSymbol(symbol_id),
            ) => {
                let runtime_thunk = RuntimeThunk::from_id(symbol_id).ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "unknown FASL runtime thunk symbol",
                    )
                })?;
                runtime_thunk.address().as_usize()
            }
            (reloc::FaslRelocKind::SideMetadata, reloc::FaslRelocTarget::SideMetadata(kind)) => {
                side_metadata_address(kind)
            }
            (reloc::FaslRelocKind::DataSlotAddress, target) => {
                data_slots.slot_rw_address(loaded, target)?
            }
            (reloc::FaslRelocKind::CacheCell, target) => {
                data_slots.slot_rw_address(loaded, target)?
            }
            (
                reloc::FaslRelocKind::CodeEntry,
                reloc::FaslRelocTarget::Entry(index) | reloc::FaslRelocTarget::Object(index),
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
                        original_bytes: relocation_original_bytes(
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
        relocation_patch_bytes(
            loaded.entrypoint,
            offset,
            &relocation.kind,
            target_address,
            relocation.addend,
            relocation_original_bytes(code_bytes, offset, &relocation.kind)?,
        )
        .map(|bytes| Some((offset, bytes)))
    }

    fn collect_code_block_data_slots(
        &mut self,
        source_index: Option<u32>,
        relocations: &[reloc::FaslRelocation],
    ) -> io::Result<CodeDataSlots<'gc>> {
        let mut slots = CodeDataSlots::default();
        for relocation in relocations {
            if let Some(index) = code_entry_keepalive_index(relocation) {
                let target = reloc::FaslRelocTarget::Object(index);
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

            if matches!(relocation.target, reloc::FaslRelocTarget::CacheCell(_)) {
                let reloc::FaslRelocTarget::CacheCell(index) = relocation.target else {
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

            if !matches!(relocation.kind, reloc::FaslRelocKind::DataSlotAddress)
                && !matches!(relocation.kind, reloc::FaslRelocKind::CacheCell)
                && !matches!(relocation.kind, reloc::FaslRelocKind::CraneliftDataSlot(_))
                && !matches!(
                    (&relocation.kind, relocation.target),
                    (
                        reloc::FaslRelocKind::Cranelift(_),
                        reloc::FaslRelocTarget::Object(_) | reloc::FaslRelocTarget::CacheCell(_)
                    )
                )
            {
                continue;
            }
            let target = relocation.target;
            let is_local_cache_cell = matches!(target, reloc::FaslRelocTarget::CacheCell(_));
            if slots.indices.contains_key(&target) {
                continue;
            }
            match target {
                reloc::FaslRelocTarget::Object(index)
                | reloc::FaslRelocTarget::CacheCell(index) => {
                    if let Some(value) = self.graph_value_optional(index)? {
                        slots.push_value(target, value)?;
                    } else {
                        slots.push_pending_value(target, index)?;
                    }
                }
                reloc::FaslRelocTarget::Entry(index) => {
                    if let Some(entrypoint) = self.graph_code_block_entrypoint_if_defined(index)? {
                        slots.push_raw(target, entrypoint)?;
                    } else {
                        slots.push_pending_entry(target, index)?;
                    }
                }
                reloc::FaslRelocTarget::SideMetadata(kind) => {
                    slots.push_raw(target, side_metadata_address(kind))?;
                }
                reloc::FaslRelocTarget::RuntimeSymbol(_) => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL runtime-symbol data slots are not supported",
                    ));
                }
            }
            if is_local_cache_cell {
                let reloc::FaslRelocTarget::CacheCell(index) = target else {
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
            let slot_index =
                data_slots.slot_index(reloc::FaslRelocTarget::CacheCell(*graph_index))?;
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
            let target = reloc::FaslRelocTarget::Entry(*code_index);
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
            let bytes = relocation_patch_bytes(
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
            unsafe {
                (fill.slot_address.as_usize() as *mut Value<'gc>).write(value);
            }
        }
        Ok(())
    }
}

fn code_entry_keepalive_index(relocation: &reloc::FaslRelocation) -> Option<u32> {
    match (&relocation.kind, relocation.target) {
        (
            reloc::FaslRelocKind::Asmkit(_) | reloc::FaslRelocKind::CodeEntry,
            reloc::FaslRelocTarget::Entry(index) | reloc::FaslRelocTarget::Object(index),
        ) => Some(index),
        (
            reloc::FaslRelocKind::Cranelift(_)
            | reloc::FaslRelocKind::DataSlotAddress
            | reloc::FaslRelocKind::CraneliftDataSlot(_),
            reloc::FaslRelocTarget::Entry(index),
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
    indices: HashMap<reloc::FaslRelocTarget, usize>,
    external_addresses: HashMap<reloc::FaslRelocTarget, Address>,
    slots: Vec<CodeDataSlot<'gc>>,
    value_bitmap: Vec<usize>,
    pending_value_indices: Vec<(reloc::FaslRelocTarget, u32)>,
    pending_entry_indices: Vec<u32>,
    local_cache_indices: Vec<u32>,
}

impl<'gc> CodeDataSlots<'gc> {
    fn push_value(&mut self, target: reloc::FaslRelocTarget, value: Value<'gc>) -> io::Result<()> {
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

    fn push_raw(&mut self, target: reloc::FaslRelocTarget, word: usize) -> io::Result<()> {
        let slot_index = self.slots.len();
        self.indices.insert(target, slot_index);
        self.slots.push(CodeDataSlot::Raw(word));
        Ok(())
    }

    fn push_pending_value(
        &mut self,
        target: reloc::FaslRelocTarget,
        graph_index: u32,
    ) -> io::Result<()> {
        self.push_value(target, Value::undefined())?;
        self.pending_value_indices.push((target, graph_index));
        Ok(())
    }

    fn push_pending_entry(
        &mut self,
        target: reloc::FaslRelocTarget,
        code_index: u32,
    ) -> io::Result<()> {
        self.push_raw(target, 0)?;
        self.pending_entry_indices.push(code_index);
        Ok(())
    }

    fn slot_index(&self, target: reloc::FaslRelocTarget) -> io::Result<usize> {
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
        target: reloc::FaslRelocTarget,
    ) -> io::Result<usize> {
        if let Some(address) = self.external_addresses.get(&target) {
            return Ok(address.as_usize());
        }
        let slot_index = self.slot_index(target)?;
        Ok(loaded.data_rw_base.as_usize() + slot_index * std::mem::size_of::<Value<'gc>>())
    }
}

fn side_metadata_address(kind: reloc::SideMetadataSlotKind) -> usize {
    match kind {
        reloc::SideMetadataSlotKind::Global => global_side_metadata_vm_base_address().as_usize(),
        reloc::SideMetadataSlotKind::VoBit => vo_bit_side_metadata_addr().as_usize(),
    }
}

fn relocation_patch_bytes(
    base: Address,
    offset: usize,
    kind: &reloc::FaslRelocKind,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    match kind {
        reloc::FaslRelocKind::Asmkit(asmkit) => {
            asmkit_relocation_patch_bytes(base, offset, *asmkit, target_address, addend)
        }
        reloc::FaslRelocKind::Cranelift(cranelift) => cranelift_relocation_patch_bytes(
            base,
            offset,
            *cranelift,
            target_address,
            addend,
            original,
        ),
        reloc::FaslRelocKind::CraneliftDataSlot(cranelift) => cranelift_relocation_patch_bytes(
            base,
            offset,
            *cranelift,
            target_address,
            addend,
            original,
        ),
        reloc::FaslRelocKind::AbsWord
        | reloc::FaslRelocKind::CodeEntry
        | reloc::FaslRelocKind::DataSlotAddress
        | reloc::FaslRelocKind::CacheCell
        | reloc::FaslRelocKind::RuntimeThunk
        | reloc::FaslRelocKind::RuntimeData
        | reloc::FaslRelocKind::SideMetadata => {
            let target = add_i64_to_usize(target_address, addend)?;
            Ok(target.to_le_bytes().to_vec())
        }
    }
}

fn relocation_original_bytes<'a>(
    code_bytes: &'a [u8],
    offset: usize,
    kind: &reloc::FaslRelocKind,
) -> io::Result<&'a [u8]> {
    let width = relocation_patch_width(kind);
    code_bytes
        .get(offset..)
        .and_then(|bytes| bytes.get(..width))
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL relocation is out of bounds",
            )
        })
}

fn relocation_patch_width(kind: &reloc::FaslRelocKind) -> usize {
    match kind {
        reloc::FaslRelocKind::Cranelift(CraneliftReloc::RiscvCallPlt)
        | reloc::FaslRelocKind::CraneliftDataSlot(CraneliftReloc::RiscvCallPlt) => 8,
        reloc::FaslRelocKind::Cranelift(
            CraneliftReloc::Abs4
            | CraneliftReloc::X86PCRel4
            | CraneliftReloc::X86CallPCRel4
            | CraneliftReloc::X86CallPLTRel4
            | CraneliftReloc::Arm64Call
            | CraneliftReloc::Aarch64AdrGotPage21
            | CraneliftReloc::Aarch64AdrPrelPgHi21
            | CraneliftReloc::Aarch64AddAbsLo12Nc
            | CraneliftReloc::Aarch64Ld64GotLo12Nc
            | CraneliftReloc::MachOAarch64TlsAdrPage21
            | CraneliftReloc::MachOAarch64TlsAdrPageOff12
            | CraneliftReloc::Aarch64TlsDescAdrPage21
            | CraneliftReloc::Aarch64TlsDescLd64Lo12
            | CraneliftReloc::Aarch64TlsDescAddLo12
            | CraneliftReloc::RiscvTlsGdHi20
            | CraneliftReloc::RiscvPCRelLo12I
            | CraneliftReloc::RiscvGotHi20
            | CraneliftReloc::RiscvPCRelHi20,
        )
        | reloc::FaslRelocKind::CraneliftDataSlot(
            CraneliftReloc::Abs4
            | CraneliftReloc::X86PCRel4
            | CraneliftReloc::X86CallPCRel4
            | CraneliftReloc::X86CallPLTRel4
            | CraneliftReloc::Arm64Call
            | CraneliftReloc::Aarch64AdrGotPage21
            | CraneliftReloc::Aarch64AdrPrelPgHi21
            | CraneliftReloc::Aarch64AddAbsLo12Nc
            | CraneliftReloc::Aarch64Ld64GotLo12Nc
            | CraneliftReloc::MachOAarch64TlsAdrPage21
            | CraneliftReloc::MachOAarch64TlsAdrPageOff12
            | CraneliftReloc::Aarch64TlsDescAdrPage21
            | CraneliftReloc::Aarch64TlsDescLd64Lo12
            | CraneliftReloc::Aarch64TlsDescAddLo12
            | CraneliftReloc::RiscvTlsGdHi20
            | CraneliftReloc::RiscvPCRelLo12I
            | CraneliftReloc::RiscvGotHi20
            | CraneliftReloc::RiscvPCRelHi20,
        ) => 4,
        reloc::FaslRelocKind::Asmkit(
            AsmkitReloc::Abs4
            | AsmkitReloc::X86PCRel4
            | AsmkitReloc::X86CallPCRel4
            | AsmkitReloc::X86CallPLTRel4,
        ) => 4,
        reloc::FaslRelocKind::Cranelift(CraneliftReloc::Aarch64TlsDescCall)
        | reloc::FaslRelocKind::CraneliftDataSlot(CraneliftReloc::Aarch64TlsDescCall) => 0,
        _ => std::mem::size_of::<usize>(),
    }
}

fn cranelift_relocation_patch_bytes(
    base: Address,
    offset: usize,
    kind: CraneliftReloc,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    match kind {
        CraneliftReloc::Abs8 => {
            let target = add_i64_to_usize(target_address, addend)?;
            Ok(target.to_le_bytes().to_vec())
        }
        CraneliftReloc::Abs4 => {
            let target = add_i64_to_usize(target_address, addend)?;
            let target = u32::try_from(target).map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL Cranelift Abs4 relocation target is out of range",
                )
            })?;
            Ok(target.to_le_bytes().to_vec())
        }
        CraneliftReloc::X86PCRel4
        | CraneliftReloc::X86CallPCRel4
        | CraneliftReloc::X86CallPLTRel4 => pcrel32_patch(base, offset, target_address, addend),
        CraneliftReloc::Arm64Call => {
            arm64_call_patch(base, offset, target_address, addend, original)
        }
        CraneliftReloc::Aarch64AdrGotPage21
        | CraneliftReloc::Aarch64AdrPrelPgHi21
        | CraneliftReloc::MachOAarch64TlsAdrPage21
        | CraneliftReloc::Aarch64TlsDescAdrPage21 => {
            aarch64_page21_patch(base, offset, target_address, addend, original)
        }
        CraneliftReloc::Aarch64AddAbsLo12Nc
        | CraneliftReloc::Aarch64TlsDescAddLo12
        | CraneliftReloc::MachOAarch64TlsAdrPageOff12 => {
            aarch64_lo12_patch(target_address, addend, original, 0)
        }
        CraneliftReloc::Aarch64Ld64GotLo12Nc | CraneliftReloc::Aarch64TlsDescLd64Lo12 => {
            aarch64_lo12_patch(target_address, addend, original, 3)
        }
        CraneliftReloc::Aarch64TlsDescCall => Ok(Vec::new()),
        CraneliftReloc::RiscvCallPlt => {
            riscv_call_plt_patch(base, offset, target_address, addend, original)
        }
        CraneliftReloc::RiscvTlsGdHi20
        | CraneliftReloc::RiscvGotHi20
        | CraneliftReloc::RiscvPCRelHi20 => {
            riscv_hi20_patch(base, offset, target_address, addend, original)
        }
        CraneliftReloc::RiscvPCRelLo12I => {
            riscv_lo12_i_patch(base, offset, target_address, addend, original)
        }
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "unsupported FASL Cranelift relocation",
        )),
    }
}

fn asmkit_relocation_patch_bytes(
    base: Address,
    offset: usize,
    kind: AsmkitReloc,
    target_address: usize,
    addend: i64,
) -> io::Result<Vec<u8>> {
    match kind {
        AsmkitReloc::Abs8 | AsmkitReloc::RiscvAbs8 => {
            let target = add_i64_to_usize(target_address, addend)?;
            Ok(target.to_le_bytes().to_vec())
        }
        AsmkitReloc::Abs4 => {
            let target = add_i64_to_usize(target_address, addend)?;
            let target = u32::try_from(target).map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL asmkit Abs4 relocation target is out of range",
                )
            })?;
            Ok(target.to_le_bytes().to_vec())
        }
        AsmkitReloc::X86PCRel4 | AsmkitReloc::X86CallPCRel4 | AsmkitReloc::X86CallPLTRel4 => {
            pcrel32_patch(base, offset, target_address, addend)
        }
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "unsupported FASL asmkit relocation",
        )),
    }
}

fn pcrel32_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
) -> io::Result<Vec<u8>> {
    let patch_address = base.as_usize().checked_add(offset).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL relocation patch address overflow",
        )
    })?;
    let target = add_i64_to_usize(target_address, addend)?;
    let displacement = target as i128 - patch_address as i128;
    let displacement = i32::try_from(displacement).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL rel32 relocation target is out of range",
        )
    })?;
    Ok(displacement.to_le_bytes().to_vec())
}

fn arm64_call_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    let patch_address = base.as_usize().checked_add(offset).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 call relocation patch address overflow",
        )
    })?;
    let target = add_i64_to_usize(target_address, addend)?;
    let displacement = target as i128 - patch_address as i128;
    if displacement % 4 != 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 call target is not instruction aligned",
        ));
    }
    let immediate = displacement / 4;
    if !(-(1i128 << 25)..(1i128 << 25)).contains(&immediate) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 call relocation target is out of range",
        ));
    }
    let instruction = read_original_u32(original)?;
    let patched = (instruction & !0x03ff_ffff) | ((immediate as u32) & 0x03ff_ffff);
    Ok(patched.to_le_bytes().to_vec())
}

fn aarch64_page21_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    let patch_address = base.as_usize().checked_add(offset).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 page relocation patch address overflow",
        )
    })?;
    let target = add_i64_to_usize(target_address, addend)?;
    let patch_page = (patch_address as i128) & !0xfffi128;
    let target_page = (target as i128) & !0xfffi128;
    let page_delta = (target_page - patch_page) >> 12;
    if !(-(1i128 << 20)..(1i128 << 20)).contains(&page_delta) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 page relocation target is out of range",
        ));
    }
    let imm = (page_delta as u32) & 0x1f_ffff;
    let immlo = imm & 0x3;
    let immhi = (imm >> 2) & 0x7ffff;
    let instruction = read_original_u32(original)?;
    let patched = (instruction & !((0x3 << 29) | (0x7ffff << 5))) | (immlo << 29) | (immhi << 5);
    Ok(patched.to_le_bytes().to_vec())
}

fn aarch64_lo12_patch(
    target_address: usize,
    addend: i64,
    original: &[u8],
    scale_shift: u32,
) -> io::Result<Vec<u8>> {
    let target = add_i64_to_usize(target_address, addend)?;
    let low = (target & 0xfff) as u32;
    if scale_shift != 0 && (low & ((1 << scale_shift) - 1)) != 0 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL AArch64 low relocation target is not aligned",
        ));
    }
    let immediate = low >> scale_shift;
    let instruction = read_original_u32(original)?;
    let patched = (instruction & !(0xfff << 10)) | (immediate << 10);
    Ok(patched.to_le_bytes().to_vec())
}

fn riscv_call_plt_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    let delta = riscv_pcrel_delta(base, offset, target_address, addend)?;
    let hi = riscv_hi20(delta)?;
    let lo = delta - (hi << 12);
    let auipc = patch_riscv_u_type(read_original_u32(&original[0..4])?, hi);
    let jalr = patch_riscv_i_type(read_original_u32(&original[4..8])?, lo)?;
    let mut bytes = Vec::with_capacity(8);
    bytes.extend_from_slice(&auipc.to_le_bytes());
    bytes.extend_from_slice(&jalr.to_le_bytes());
    Ok(bytes)
}

fn riscv_hi20_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    let delta = riscv_pcrel_delta(base, offset, target_address, addend)?;
    let instruction = patch_riscv_u_type(read_original_u32(original)?, riscv_hi20(delta)?);
    Ok(instruction.to_le_bytes().to_vec())
}

fn riscv_lo12_i_patch(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
    original: &[u8],
) -> io::Result<Vec<u8>> {
    let delta = riscv_pcrel_delta(base, offset, target_address, addend)?;
    let hi = riscv_hi20(delta)?;
    let lo = delta - (hi << 12);
    let instruction = patch_riscv_i_type(read_original_u32(original)?, lo)?;
    Ok(instruction.to_le_bytes().to_vec())
}

fn riscv_pcrel_delta(
    base: Address,
    offset: usize,
    target_address: usize,
    addend: i64,
) -> io::Result<i128> {
    let patch_address = base.as_usize().checked_add(offset).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL RISC-V relocation patch address overflow",
        )
    })?;
    let target = add_i64_to_usize(target_address, addend)?;
    Ok(target as i128 - patch_address as i128)
}

fn riscv_hi20(delta: i128) -> io::Result<i128> {
    let hi = (delta + 0x800) >> 12;
    if !(-(1i128 << 19)..(1i128 << 19)).contains(&hi) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL RISC-V hi20 relocation target is out of range",
        ));
    }
    Ok(hi)
}

fn patch_riscv_u_type(instruction: u32, hi: i128) -> u32 {
    (instruction & 0x00000fff) | (((hi as u32) & 0x000f_ffff) << 12)
}

fn patch_riscv_i_type(instruction: u32, lo: i128) -> io::Result<u32> {
    if !(-2048..2048).contains(&lo) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL RISC-V lo12 relocation target is out of range",
        ));
    }
    Ok((instruction & 0x000f_ffff) | (((lo as u32) & 0xfff) << 20))
}

fn read_original_u32(original: &[u8]) -> io::Result<u32> {
    let bytes: [u8; 4] = original
        .get(0..4)
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL relocation is out of bounds",
            )
        })?
        .try_into()
        .expect("slice length was checked");
    Ok(u32::from_le_bytes(bytes))
}

fn add_i64_to_usize(value: usize, addend: i64) -> io::Result<usize> {
    if addend >= 0 {
        let addend = usize::try_from(addend).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL code block relocation addend is too large",
            )
        })?;
        value.checked_add(addend)
    } else {
        let addend = usize::try_from(addend.unsigned_abs()).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "FASL code block relocation addend is too small",
            )
        })?;
        value.checked_sub(addend)
    }
    .ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "FASL code block relocation addend overflow",
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::fasl::reloc::FaslRelocKind;
    use cranelift_codegen::binemit::Reloc;

    #[test]
    fn cranelift_arm64_call_relocation_patches_branch_immediate() {
        let base = unsafe { Address::from_usize(0x1_0000) };
        let original = 0x9400_0000u32.to_le_bytes();

        let bytes = relocation_patch_bytes(
            base,
            0,
            &FaslRelocKind::Cranelift(Reloc::Arm64Call),
            0x1_0040,
            0,
            &original,
        )
        .expect("patch arm64 call");

        assert_eq!(u32::from_le_bytes(bytes.try_into().unwrap()), 0x9400_0010);
    }

    #[test]
    fn cranelift_aarch64_page21_relocation_patches_adrp_immediate() {
        let base = unsafe { Address::from_usize(0x1_0000) };
        let original = 0x9000_0000u32.to_le_bytes();

        let bytes = relocation_patch_bytes(
            base,
            0,
            &FaslRelocKind::Cranelift(Reloc::Aarch64AdrPrelPgHi21),
            0x1_3000,
            0,
            &original,
        )
        .expect("patch aarch64 adrp");

        assert_eq!(u32::from_le_bytes(bytes.try_into().unwrap()), 0xF000_0000);
    }

    #[test]
    fn cranelift_riscv_call_plt_relocation_patches_auipc_jalr_pair() {
        let base = unsafe { Address::from_usize(0x1_0000) };
        let mut original = Vec::new();
        original.extend_from_slice(&0x0000_0097u32.to_le_bytes());
        original.extend_from_slice(&0x0000_8080u32.to_le_bytes());

        let bytes = relocation_patch_bytes(
            base,
            0,
            &FaslRelocKind::Cranelift(Reloc::RiscvCallPlt),
            0x1_1804,
            0,
            &original,
        )
        .expect("patch riscv call");

        assert_eq!(
            u32::from_le_bytes(bytes[0..4].try_into().unwrap()),
            0x0000_2097
        );
        assert_eq!(
            u32::from_le_bytes(bytes[4..8].try_into().unwrap()),
            0x8040_8080
        );
    }
}
