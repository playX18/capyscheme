//! FASL (Fast Loading) format
//!
//! Serialzies/deserializes Scheme values to/from a binary format
//! that can be loaded quickly at runtime.

use std::{
    collections::{BTreeMap, HashMap},
    io::{self, BufReader, BufWriter, Cursor, Read, Write},
};

use asmkit::core::buffer::Reloc as AsmkitReloc;
use flate2::{Compression, read::GzDecoder, write::GzEncoder};
use mmtk::util::metadata::side_metadata::{
    global_side_metadata_vm_base_address, vo_bit_side_metadata_addr,
};

use crate::{
    prelude::Keyword,
    rsgc::{Gc, mmtk::util::Address},
};
use im::HashSet;

use crate::runtime::{
    Context,
    code_memory::{CodeAllocation, runtime_code_memory},
    symbols::{RuntimeData, RuntimeThunk},
    value::{
        BigInt, ByteVector, Closure, CodeArity, CodeBlock, Complex, HashTable, HashTableType,
        IntoValue, Pair, Rational, Str, Symbol, Tuple, Value, Vector,
    },
    vm::syntax::Syntax,
};

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

#[path = "fasl/graph.rs"]
pub mod graph;
#[path = "fasl/reloc.rs"]
pub mod reloc;

/// Reference to an object.
pub const FASL_TAG_REF: u8 = 0xFF;
pub const FASL_TAG_REF_INIT: u8 = 0xFE;

pub struct FASLWriter<'gc, W: Write> {
    pub ctx: Context<'gc>,
    pub writer: BufWriter<W>,
    pub lites: Gc<'gc, HashTable<'gc>>,
    pub stack: Vec<Value<'gc>>,
    pub reference_map: BTreeMap<Address, u32>,
    pub initmap: HashSet<Address>,
}

pub struct FaslCodeBlockSpec<'a, 'gc> {
    pub bytes: &'a [u8],
    pub entry_offset: u32,
    pub arity: i32,
    pub is_cont: bool,
    pub metadata: Value<'gc>,
    pub relocations: &'a [reloc::FaslRelocation],
}

pub struct FaslClosureSpec<'a, 'gc> {
    pub code: FaslCodeBlockSpec<'a, 'gc>,
    pub free: &'a [Value<'gc>],
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
    pub value: Value<'gc>,
}

pub struct FaslGraphCodeBlockSpec<'a, 'gc> {
    pub index: u32,
    pub code: FaslCodeBlockSpec<'a, 'gc>,
}

impl<'gc, W: Write> FASLWriter<'gc, W> {
    pub fn put8(&mut self, byte: u8) -> io::Result<()> {
        self.writer.write_all(&[byte])
    }

    pub fn put16(&mut self, value: u16) -> io::Result<()> {
        self.writer.write_all(&value.to_le_bytes())
    }

    pub fn put32(&mut self, value: u32) -> io::Result<()> {
        self.writer.write_all(&value.to_le_bytes())
    }

    pub fn put64(&mut self, value: u64) -> io::Result<()> {
        self.writer.write_all(&value.to_le_bytes())
    }

    pub fn put_many(&mut self, values: impl AsRef<[u8]>) -> io::Result<()> {
        self.writer.write_all(values.as_ref())
    }

    pub fn push(&mut self, value: Value<'gc>) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Option<Value<'gc>> {
        self.stack.pop()
    }

    pub fn scan(&mut self, obj: Value<'gc>) -> io::Result<()> {
        if obj.is_immediate() || obj.is_flonum() {
            return Ok(());
        }

        if obj.is::<Symbol>() || obj.is::<Str>() {
            if self.lites.contains_key(self.ctx, obj) {
                return Ok(());
            }

            let nsize = self.lites.len();
            self.lites.put(self.ctx, obj, Value::new(nsize as i32));
            return Ok(());
        }

        if let Some(_) = self.reference_map.get(&obj.as_cell_raw().to_address()) {
            return Ok(());
        }

        let ref_id = self.reference_map.len() as u32;
        self.reference_map
            .insert(obj.as_cell_raw().to_address(), ref_id);

        if obj.is_pair() {
            self.scan(obj.car())?;
            self.scan(obj.cdr())?;
            return Ok(());
        }

        if obj.is::<Tuple>() {
            let tuple = obj.downcast::<Tuple>();
            for item in tuple.iter() {
                self.scan(item.get())?;
            }
            return Ok(());
        }

        if obj.is::<Vector>() {
            let vec = obj.downcast::<Vector>();
            for item in vec.iter() {
                self.scan(item.get())?;
            }
            return Ok(());
        }

        if obj.is::<ByteVector>() {
            return Ok(());
        }

        if obj.is::<Syntax>() {
            let syntax = obj.downcast::<Syntax>();
            self.scan(syntax.expr())?;
            self.scan(syntax.module())?;
            self.scan(syntax.source())?;
            self.scan(syntax.wrap())?;
            return Ok(());
        }

        if obj.is::<Complex>() {
            let cn = obj.downcast::<Complex>();
            self.scan(cn.real.into_value(self.ctx))?;
            self.scan(cn.imag.into_value(self.ctx))?;
            return Ok(());
        }

        if obj.is::<Rational>() {
            let rn = obj.downcast::<Rational>();
            self.scan(rn.numerator.into_value(self.ctx))?;
            self.scan(rn.denominator.into_value(self.ctx))?;
            return Ok(());
        }

        if obj.is_number() {
            return Ok(());
        }

        if obj.is::<Keyword>() {
            let keyword = obj.downcast::<Keyword>();
            self.scan(keyword.symbol.into())?;
            return Ok(());
        }

        println!("{obj}");
        return Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "Unsupported type for FASL serialization",
        ));
    }

    pub fn put(&mut self, obj: Value<'gc>) -> io::Result<()> {
        if obj.is_immediate() {
            if obj.is_int32() {
                self.put8(FASL_TAG_FIXNUM)?;
                self.put32(obj.as_int32() as u32)?;
            } else if obj.is_bool() {
                self.put8(if obj.as_bool() {
                    FASL_TAG_T
                } else {
                    FASL_TAG_F
                })?;
            } else if obj.is_null() {
                self.put8(FASL_TAG_NIL)?;
            } else if obj.is_char() {
                self.put8(FASL_TAG_CHAR)?;
                self.put32(obj.char() as u32)?;
            } else if obj.is_flonum() {
                self.put8(FASL_TAG_FLONUM)?;
                self.put64(obj.as_flonum().to_bits())?;
            } else {
                self.put8(FASL_TAG_IMMEDIATE)?;
                self.put64(obj.bits())?;
            }

            return Ok(());
        }

        if obj.is::<Symbol>() || obj.is::<Str>() {
            let id = self.lites.get(self.ctx, obj).unwrap().as_int32();
            self.put8(FASL_TAG_LOOKUP)?;
            self.put32(id as u32)?;
            return Ok(());
        }

        if self.initmap.contains(&obj.as_cell_raw().to_address()) {
            let ref_id = self.reference_map[&obj.as_cell_raw().to_address()];
            self.put8(FASL_TAG_REF)?;
            self.put32(ref_id)?;
            return Ok(());
        }

        self.initmap.insert(obj.as_cell_raw().to_address());

        self.put8(FASL_TAG_REF_INIT)?;
        let ix = self.reference_map[&obj.as_cell_raw().to_address()];
        self.put32(ix)?;

        if obj.is_pair() {
            self.put_list(obj)?;
            return Ok(());
        }

        if obj.is::<Vector>() {
            let vec = obj.downcast::<Vector>();
            self.put8(FASL_TAG_VECTOR)?;
            self.put32(vec.len() as u32)?;
            for item in vec.iter() {
                self.put(item.get())?;
            }
            return Ok(());
        }

        if obj.is::<Tuple>() {
            let tuple = obj.downcast::<Tuple>();
            self.put8(FASL_TAG_TUPLE)?;
            self.put32(tuple.len() as u32)?;
            for item in tuple.iter() {
                self.put(item.get())?;
            }
            return Ok(());
        }

        if obj.is::<ByteVector>() {
            let bvec = obj.downcast::<ByteVector>();
            self.put8(FASL_TAG_BVECTOR)?;
            self.put64(bvec.len() as u64)?;
            self.put_many(bvec.as_slice())?;
            return Ok(());
        }

        if obj.is::<BigInt>() {
            let bigint = obj.downcast::<BigInt>();
            self.put8(FASL_TAG_BIGINT)?;
            self.put8(bigint.negative() as u8)?;
            self.put32(bigint.len() as u32)?;
            for digit in bigint.iter() {
                self.put64(*digit)?;
            }
            return Ok(());
        }

        if obj.is::<Complex>() {
            let complex = obj.downcast::<Complex>();
            self.put8(FASL_TAG_COMPLEX)?;

            self.put(complex.real.into_value(self.ctx))?;
            self.put(complex.imag.into_value(self.ctx))?;
            return Ok(());
        }

        if obj.is::<Rational>() {
            let rational = obj.downcast::<Rational>();
            self.put8(FASL_TAG_RATIONAL)?;
            self.put(rational.numerator.into_value(self.ctx))?;
            self.put(rational.denominator.into_value(self.ctx))?;
            return Ok(());
        }

        if obj.is::<Syntax>() {
            let syntax = obj.downcast::<Syntax>();
            self.put8(FASL_TAG_SYNTAX)?;
            self.put(syntax.expr())?;
            self.put(syntax.module())?;
            self.put(syntax.source())?;
            self.put(syntax.wrap())?;
            self.put(syntax.properties())?;
            return Ok(());
        }

        if obj.is::<Keyword>() {
            let keyword = obj.downcast::<Keyword>();
            let sym = keyword.to_symbol();
            self.put8(FASL_TAG_KEYWORD)?;
            self.put(sym.into_value(self.ctx))?;
            return Ok(());
        }
        println!("{obj}");
        return Err(io::Error::new(
            io::ErrorKind::Unsupported,
            format!("Unsupported type for FASL serialization: {}", obj),
        ));
    }

    pub fn put_list(&mut self, mut obj: Value<'gc>) -> io::Result<()> {
        let mut count = 0;

        while obj.is_pair() {
            self.push(obj.car());
            obj = obj.cdr();
            count += 1;
        }

        if obj.is_null() {
            self.put8(FASL_TAG_PLIST)?;
            self.put32(count as u32)?;
        } else {
            self.put8(FASL_TAG_DLIST)?;
            self.put32(count as u32)?;
            self.put(obj)?;
        }

        while count > 0 {
            count -= 1;
            let item = self.pop().unwrap();

            self.put(item)?;
        }

        Ok(())
    }

    pub fn put_lites(&mut self) -> io::Result<()> {
        self.put32(self.lites.len() as u32)?;

        for (key, value) in self.lites.iter() {
            self.put32(value.as_int32() as u32)?;

            if key.is::<Symbol>() {
                let sym = key.downcast::<Symbol>();

                if sym.is_interned() {
                    self.put8(FASL_TAG_SYMBOL)?;
                } else {
                    self.put8(FASL_TAG_UNINTERNED_SYMBOL)?;
                }
                let str = sym.to_string();
                let bytes = str.as_bytes();
                self.put32(bytes.len() as u32)?;
                self.put_many(bytes)?;
            } else if key.is::<Str>() {
                let str = key.downcast::<Str>();
                self.put8(FASL_TAG_STR)?;
                let str = str.to_string();
                let bytes = str.as_bytes();
                self.put32(bytes.len() as u32)?;
                self.put_many(bytes)?;
            } else {
                println!("CAN'T SERIALIZE LITE: {key}");
                return Err(io::Error::new(
                    io::ErrorKind::Unsupported,
                    "Unsupported type for FASL serialization",
                ));
            }
        }

        Ok(())
    }

    pub fn write(mut self, obj: Value<'gc>) -> io::Result<()> {
        self.scan(obj)?;
        self.put_lites()?;
        let mut payload = Vec::new();
        {
            let mut writer = self.payload_writer(&mut payload);
            writer.put(obj)?;
            writer.writer.flush()?;
        }
        self.put_uncompressed_group(&payload)?;
        self.writer.flush()?;
        Ok(())
    }

    pub fn write_gzip(mut self, obj: Value<'gc>) -> io::Result<()> {
        self.scan(obj)?;
        self.put_lites()?;
        let mut payload = Vec::new();
        {
            let mut writer = self.payload_writer(&mut payload);
            writer.put(obj)?;
            writer.writer.flush()?;
        }
        let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
        encoder.write_all(&payload)?;
        let compressed = encoder.finish()?;

        let mut pcfasl = Vec::new();
        pcfasl.push(FASL_TAG_GZIP);
        pcfasl.extend_from_slice(&checked_u32_len(payload.len())?.to_le_bytes());
        pcfasl.extend_from_slice(&checked_u32_len(compressed.len())?.to_le_bytes());
        pcfasl.extend_from_slice(&compressed);
        self.put_group(FASL_SITUATION_VISIT_REVISIT, &pcfasl)?;
        self.writer.flush()?;
        Ok(())
    }

    pub fn write_loaded_closure(mut self, spec: &FaslClosureSpec<'_, 'gc>) -> io::Result<()> {
        self.scan(spec.code.metadata)?;
        for value in spec.free {
            self.scan(*value)?;
        }
        self.put_lites()?;
        let mut payload = Vec::new();
        {
            let mut writer = self.payload_writer(&mut payload);
            writer.put8(FASL_TAG_GRAPH)?;
            writer.put32(1)?;
            writer.put32(0)?;
            writer.put_closure(spec)?;
            writer.writer.flush()?;
        }
        self.put_uncompressed_group(&payload)?;
        self.writer.flush()?;
        Ok(())
    }

    pub fn write_loaded_program(mut self, spec: &FaslProgramSpec<'_, 'gc>) -> io::Result<()> {
        for value in spec.values {
            self.scan(value.value)?;
        }
        for code in spec.code_blocks {
            self.scan(code.code.metadata)?;
        }
        self.put_lites()?;
        let mut payload = Vec::new();
        {
            let mut writer = self.payload_writer(&mut payload);
            writer.put8(FASL_TAG_GRAPH)?;
            writer.put32(spec.graph_len)?;
            writer.put32(0)?;
            let item_count = checked_u32_len(spec.values.len() + spec.code_blocks.len() + 1)?;
            writer.put8(FASL_TAG_BEGIN)?;
            writer.put32(item_count)?;
            for value in spec.values {
                writer.put8(FASL_TAG_GRAPH_DEF)?;
                writer.put32(value.index)?;
                writer.put(value.value)?;
            }
            for code in spec.code_blocks {
                writer.put8(FASL_TAG_GRAPH_DEF)?;
                writer.put32(code.index)?;
                writer.put_code_block(&code.code)?;
            }
            writer.put_entry_closure(spec.entry_code_index, spec.entry_is_cont)?;
            writer.writer.flush()?;
        }
        self.put_uncompressed_group(&payload)?;
        self.writer.flush()?;
        Ok(())
    }

    fn payload_writer<'a>(&self, writer: &'a mut Vec<u8>) -> FASLWriter<'gc, &'a mut Vec<u8>> {
        FASLWriter {
            ctx: self.ctx,
            writer: BufWriter::new(writer),
            lites: self.lites,
            stack: Vec::new(),
            reference_map: self.reference_map.clone(),
            initmap: HashSet::new(),
        }
    }

    fn put_uncompressed_group(&mut self, payload: &[u8]) -> io::Result<()> {
        let mut pcfasl = Vec::new();
        pcfasl.push(FASL_TAG_UNCOMPRESSED);
        pcfasl.extend_from_slice(&checked_u32_len(payload.len())?.to_le_bytes());
        pcfasl.extend_from_slice(payload);
        self.put_group(FASL_SITUATION_VISIT_REVISIT, &pcfasl)
    }

    fn put_group(&mut self, situation: u8, pcfasl: &[u8]) -> io::Result<()> {
        self.put8(FASL_TAG_GROUP)?;
        self.put8(situation)?;
        self.put32(checked_u32_len(pcfasl.len())?)?;
        self.writer.write_all(pcfasl)
    }

    fn put_closure(&mut self, spec: &FaslClosureSpec<'_, 'gc>) -> io::Result<()> {
        self.put8(FASL_TAG_CLOSURE)?;
        self.put8(FASL_TAG_GRAPH_DEF)?;
        self.put32(0)?;
        self.put_code_block(&spec.code)?;
        self.put32(checked_u32_len(spec.free.len())?)?;
        for value in spec.free {
            self.put(*value)?;
        }
        self.put8(u8::from(spec.is_cont))
    }

    fn put_entry_closure(&mut self, entry_code_index: u32, is_cont: bool) -> io::Result<()> {
        self.put8(FASL_TAG_CLOSURE)?;
        self.put8(FASL_TAG_ENTRY)?;
        self.put32(entry_code_index)?;
        self.put32(0)?;
        self.put8(u8::from(is_cont))
    }

    fn put_code_block(&mut self, spec: &FaslCodeBlockSpec<'_, 'gc>) -> io::Result<()> {
        self.put8(FASL_TAG_CODE_BLOCK)?;
        self.put32(checked_u32_len(spec.bytes.len())?)?;
        self.put_many(spec.bytes)?;
        self.put32(spec.entry_offset)?;
        self.put32(spec.arity as u32)?;
        self.put8(u8::from(spec.is_cont))?;
        self.put(spec.metadata)?;
        self.put32(checked_u32_len(spec.relocations.len())?)?;
        for relocation in spec.relocations {
            relocation.encode(&mut self.writer)?;
        }
        Ok(())
    }

    pub fn new(ctx: Context<'gc>, writer: W) -> Self {
        Self {
            ctx,
            writer: BufWriter::new(writer),
            lites: HashTable::new(*ctx, HashTableType::Eq, 32, 0.75),
            stack: Vec::new(),
            reference_map: BTreeMap::new(),
            initmap: HashSet::new(),
        }
    }
}

fn checked_u32_len(len: usize) -> io::Result<u32> {
    u32::try_from(len)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "FASL payload is too large"))
}

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
    pub reference_map: HashMap<u32, Address>,
    graph_stack: Vec<graph::FaslGraphTable<'gc>>,
    shared_data_slots: Vec<HashMap<reloc::FaslRelocTarget, usize>>,
    pending_code_entry_relocations: Vec<Vec<PendingCodeEntryRelocation>>,
    pending_data_slot_fills: Vec<Vec<PendingDataSlotFill>>,
    pending_code_entry_slot_fills: Vec<Vec<PendingCodeEntrySlotFill>>,
}

#[derive(Clone)]
struct PendingCodeEntryRelocation {
    handle: u32,
    base: Address,
    offset: usize,
    target_index: u32,
    kind: reloc::FaslRelocKind,
    addend: i64,
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

                let kw = Keyword::from_symbol(*self.ctx, sym);
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
                self.shared_data_slots.push(HashMap::new());
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
                self.shared_data_slots
                    .pop()
                    .expect("shared data slot stack should match graph stack");
                self.graph_stack.pop();
                if value.is_ok()
                    && (!pending.is_empty()
                        || !pending_data.is_empty()
                        || !pending_entry_slots.is_empty())
                {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        if !pending.is_empty() {
                            "unresolved FASL code entry relocation"
                        } else if !pending_entry_slots.is_empty() {
                            "unresolved FASL code entry data slot fill"
                        } else {
                            "unresolved FASL data slot fill"
                        },
                    ));
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
                let code_len = self.read32()? as usize;
                let mut bytes = vec![0; code_len];
                self.reader.read_exact(&mut bytes)?;
                let entry_offset = self.read32()? as usize;
                if bytes.is_empty() || entry_offset >= bytes.len() {
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
                let data_slots = self.collect_code_block_data_slots(&relocations)?;
                let loaded = runtime_code_memory()
                    .lock()
                    .unwrap()
                    .allocate_copy_with_data_slots(&bytes, data_slots.slots.len())?;
                self.register_shared_data_slots(loaded, &data_slots)?;
                self.initialize_loaded_data_slots(loaded, &data_slots);
                self.add_pending_data_slot_fills(loaded, &data_slots)?;
                self.add_pending_code_entry_slot_fills(loaded, &data_slots)?;
                self.apply_code_block_relocations(loaded, &relocations, &data_slots)?;
                let code_block = CodeBlock::new_loaded_with_data(
                    self.ctx,
                    loaded.entrypoint + entry_offset,
                    CodeArity::new(arity),
                    is_cont,
                    metadata,
                    loaded.handle,
                    loaded.data_rw_base,
                    loaded.data_len as u32,
                    &data_slots.value_bitmap,
                );
                Ok(Value::from(code_block))
            }

            _x @ FASL_TAG_CLOSURE => {
                let code_block_value = self.read_value()?;
                let code_block = code_block_value.try_as::<CodeBlock>().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "FASL closure code is not a code block",
                    )
                })?;
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

    pub fn read(mut self) -> io::Result<Value<'gc>> {
        self.read_lites()?;
        let value = self.read_value()?;

        Ok(value)
    }

    pub fn new(ctx: Context<'gc>, reader: R) -> Self {
        Self {
            ctx,
            reader: BufReader::new(reader),
            lites: HashTable::new(*ctx, HashTableType::Eq, 32, 0.75),
            reference_map: HashMap::new(),
            graph_stack: Vec::new(),
            shared_data_slots: Vec::new(),
            pending_code_entry_relocations: Vec::new(),
            pending_data_slot_fills: Vec::new(),
            pending_code_entry_slot_fills: Vec::new(),
        }
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
        let value = reader.read_value()?;
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
            let value = self.read_tagged_value(tag)?;
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
        loaded: CodeAllocation,
        relocations: &[reloc::FaslRelocation],
        data_slots: &CodeDataSlots<'gc>,
    ) -> io::Result<()> {
        let mut patches = Vec::with_capacity(relocations.len());
        for relocation in relocations {
            if let Some(patch) =
                self.resolve_code_block_relocation_patch(loaded, relocation, data_slots)?
            {
                patches.push(patch);
            }
        }
        let patch_refs = patches
            .iter()
            .map(|(offset, bytes)| (*offset, bytes.as_slice()))
            .collect::<Vec<_>>();
        runtime_code_memory()
            .lock()
            .unwrap()
            .patch_many(loaded.handle, &patch_refs)?;
        Ok(())
    }

    fn resolve_code_block_relocation_patch(
        &mut self,
        loaded: CodeAllocation,
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
                    self.add_pending_code_entry_relocation(PendingCodeEntryRelocation {
                        handle: loaded.handle,
                        base: loaded.entrypoint,
                        offset,
                        target_index: index,
                        kind: relocation.kind.clone(),
                        addend: relocation.addend,
                    })?;
                    return Ok(None);
                }
            },
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
            (
                reloc::FaslRelocKind::CodeEntry,
                reloc::FaslRelocTarget::Entry(index) | reloc::FaslRelocTarget::Object(index),
            ) => match self.graph_code_block_entrypoint_if_defined(index)? {
                Some(entrypoint) => entrypoint,
                None => {
                    self.add_pending_code_entry_relocation(PendingCodeEntryRelocation {
                        handle: loaded.handle,
                        base: loaded.entrypoint,
                        offset,
                        target_index: index,
                        kind: relocation.kind.clone(),
                        addend: relocation.addend,
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
        )
        .map(|bytes| Some((offset, bytes)))
    }

    fn collect_code_block_data_slots(
        &mut self,
        relocations: &[reloc::FaslRelocation],
    ) -> io::Result<CodeDataSlots<'gc>> {
        let mut slots = CodeDataSlots::default();
        for relocation in relocations {
            if !matches!(relocation.kind, reloc::FaslRelocKind::DataSlotAddress) {
                continue;
            }
            let target = relocation.target;
            if let Some(address) = self
                .shared_data_slots
                .last()
                .and_then(|shared| shared.get(&target))
                .copied()
            {
                slots.external_addresses.insert(target, address);
                continue;
            }
            if slots.indices.contains_key(&target) {
                continue;
            }
            match target {
                reloc::FaslRelocTarget::Object(index) => {
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
        }
        Ok(slots)
    }

    fn register_shared_data_slots(
        &mut self,
        loaded: CodeAllocation,
        data_slots: &CodeDataSlots<'gc>,
    ) -> io::Result<()> {
        if data_slots.slots.is_empty() {
            return Ok(());
        }
        let Some(shared) = self.shared_data_slots.last_mut() else {
            return Ok(());
        };
        for (target, index) in &data_slots.indices {
            if data_slots.external_addresses.contains_key(target) {
                continue;
            }
            let address =
                loaded.data_rw_base.as_usize() + index * std::mem::size_of::<Value<'gc>>();
            shared.insert(*target, address);
        }
        Ok(())
    }

    fn initialize_loaded_data_slots(
        &self,
        loaded: CodeAllocation,
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
        loaded: CodeAllocation,
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
        for graph_index in &data_slots.pending_value_indices {
            let target = reloc::FaslRelocTarget::Object(*graph_index);
            let slot_index = data_slots.slot_index(target)?;
            pending.push(PendingDataSlotFill {
                target_index: *graph_index,
                slot_address: loaded.data_rw_base + slot_index * std::mem::size_of::<Value<'gc>>(),
            });
        }
        Ok(())
    }

    fn add_pending_code_entry_slot_fills(
        &mut self,
        loaded: CodeAllocation,
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
        let mut patches = Vec::with_capacity(resolved.len());
        for relocation in resolved {
            let bytes = relocation_patch_bytes(
                relocation.base,
                relocation.offset,
                &relocation.kind,
                target,
                relocation.addend,
            )?;
            patches.push((relocation.handle, relocation.offset, bytes));
        }
        let mut memory = runtime_code_memory().lock().unwrap();
        for (handle, offset, bytes) in &patches {
            memory.patch(*handle, *offset, bytes)?;
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
    external_addresses: HashMap<reloc::FaslRelocTarget, usize>,
    slots: Vec<CodeDataSlot<'gc>>,
    value_bitmap: Vec<usize>,
    pending_value_indices: Vec<u32>,
    pending_entry_indices: Vec<u32>,
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
        self.pending_value_indices.push(graph_index);
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
        loaded: CodeAllocation,
        target: reloc::FaslRelocTarget,
    ) -> io::Result<usize> {
        if let Some(address) = self.external_addresses.get(&target).copied() {
            return Ok(address);
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
) -> io::Result<Vec<u8>> {
    match kind {
        reloc::FaslRelocKind::Asmkit(asmkit) => {
            asmkit_relocation_patch_bytes(base, offset, *asmkit, target_address, addend)
        }
        reloc::FaslRelocKind::AbsWord
        | reloc::FaslRelocKind::CodeEntry
        | reloc::FaslRelocKind::DataSlotAddress
        | reloc::FaslRelocKind::RuntimeThunk
        | reloc::FaslRelocKind::RuntimeData
        | reloc::FaslRelocKind::SideMetadata => {
            let target = add_i64_to_usize(target_address, addend)?;
            Ok(target.to_le_bytes().to_vec())
        }
        reloc::FaslRelocKind::CacheCell => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "unsupported FASL code block relocation",
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
            let patch_address = base.as_usize().checked_add(offset).ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL asmkit relocation patch address overflow",
                )
            })?;
            let target = add_i64_to_usize(target_address, addend)?;
            let displacement = target as i128 - patch_address as i128;
            let displacement = i32::try_from(displacement).map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "FASL asmkit rel32 relocation target is out of range",
                )
            })?;
            Ok(displacement.to_le_bytes().to_vec())
        }
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "unsupported FASL asmkit relocation",
        )),
    }
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
mod unified_fasl_tests {
    use super::{
        FASL_SITUATION_VISIT_REVISIT, FASL_TAG_CLOSURE, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF,
        FASL_TAG_GROUP, FASL_TAG_UNCOMPRESSED, FASLReader,
    };

    #[test]
    fn fasl_writer_emits_visit_revisit_uncompressed_group() {
        use super::FASLWriter;
        use crate::runtime::{Scheme, value::Value};

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            FASLWriter::new(ctx, &mut bytes)
                .write(Value::new(42))
                .expect("write grouped FASL");

            assert_eq!(&bytes[0..4], &0u32.to_le_bytes()); // lites
            assert_eq!(bytes[4], FASL_TAG_GROUP);
            assert_eq!(bytes[5], FASL_SITUATION_VISIT_REVISIT);
            let group_size = u32::from_le_bytes(bytes[6..10].try_into().unwrap()) as usize;
            assert_eq!(group_size, bytes.len() - 10);
            assert_eq!(bytes[10], FASL_TAG_UNCOMPRESSED);
            let payload_size = u32::from_le_bytes(bytes[11..15].try_into().unwrap()) as usize;
            assert_eq!(payload_size, bytes.len() - 15);
        });
    }

    #[test]
    fn fasl_reader_loads_gzip_wrapped_value() {
        use super::FASLWriter;
        use crate::runtime::{Scheme, value::Value};

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            FASLWriter::new(ctx, &mut bytes)
                .write_gzip(Value::new(42))
                .expect("write gzip FASL");

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("read gzip FASL");
            assert_eq!(value, Value::new(42));
        });
    }

    #[test]
    fn fasl_relocation_records_roundtrip_all_current_target_families() {
        use super::reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation, SideMetadataSlotKind};
        use asmkit::core::buffer::Reloc as AsmkitReloc;

        let relocations = vec![
            FaslRelocation {
                offset: 4,
                kind: FaslRelocKind::Asmkit(AsmkitReloc::X86CallPCRel4),
                target: FaslRelocTarget::Entry(7),
                addend: -4,
            },
            FaslRelocation {
                offset: 16,
                kind: FaslRelocKind::AbsWord,
                target: FaslRelocTarget::Object(3),
                addend: 8,
            },
            FaslRelocation {
                offset: 20,
                kind: FaslRelocKind::CodeEntry,
                target: FaslRelocTarget::Entry(5),
                addend: 0,
            },
            FaslRelocation {
                offset: 24,
                kind: FaslRelocKind::DataSlotAddress,
                target: FaslRelocTarget::Object(9),
                addend: 16,
            },
            FaslRelocation {
                offset: 32,
                kind: FaslRelocKind::RuntimeThunk,
                target: FaslRelocTarget::RuntimeSymbol(11),
                addend: 0,
            },
            FaslRelocation {
                offset: 40,
                kind: FaslRelocKind::RuntimeData,
                target: FaslRelocTarget::RuntimeSymbol(12),
                addend: -8,
            },
            FaslRelocation {
                offset: 48,
                kind: FaslRelocKind::SideMetadata,
                target: FaslRelocTarget::SideMetadata(SideMetadataSlotKind::VoBit),
                addend: 0,
            },
            FaslRelocation {
                offset: 56,
                kind: FaslRelocKind::CacheCell,
                target: FaslRelocTarget::Object(13),
                addend: 0,
            },
        ];

        let mut encoded = Vec::new();
        for relocation in &relocations {
            relocation.encode(&mut encoded).expect("encode relocation");
        }

        let mut input = std::io::Cursor::new(encoded.as_slice());
        let decoded = (0..relocations.len())
            .map(|_| FaslRelocation::decode(&mut input))
            .collect::<Result<Vec<_>, _>>()
            .expect("decode relocations");

        assert_eq!(decoded, relocations);
        assert_eq!(input.position(), encoded.len() as u64);
    }

    fn put_u32(out: &mut Vec<u8>, value: u32) {
        out.extend_from_slice(&value.to_le_bytes());
    }

    #[test]
    fn graph_def_and_ref_preserve_shared_objects() {
        use super::{
            FASL_TAG_BVECTOR, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_GRAPH_REF,
            FASL_TAG_VECTOR, FASLReader,
        };
        use crate::runtime::{Scheme, value::Vector};

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 1); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 2);
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_BVECTOR);
            bytes.extend_from_slice(&3u64.to_le_bytes());
            bytes.extend_from_slice(&[1, 2, 3]);
            bytes.push(FASL_TAG_GRAPH_REF);
            put_u32(&mut bytes, 0);

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("read graph fasl");
            let vector = value.downcast::<Vector>();
            let first = vector[0].get();
            let second = vector[1].get();

            assert_eq!(first, second);
        });
    }

    #[test]
    fn graph_def_vector_can_reference_itself() {
        use super::{
            FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_GRAPH_REF, FASL_TAG_VECTOR, FASLReader,
        };
        use crate::{
            rsgc::Gc,
            runtime::{Scheme, value::Vector},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 1); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 1);
            bytes.push(FASL_TAG_GRAPH_REF);
            put_u32(&mut bytes, 0);

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("read cyclic vector graph");
            let vector = value.downcast::<Vector>();

            assert!(Gc::ptr_eq(vector[0].get().downcast::<Vector>(), vector));
        });
    }

    #[test]
    fn graph_def_dotted_pair_can_reference_itself() {
        use super::{
            FASL_TAG_DLIST, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_GRAPH_REF, FASLReader,
        };
        use crate::{
            rsgc::Gc,
            runtime::{Scheme, value::Pair},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 1); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_DLIST);
            put_u32(&mut bytes, 1);
            bytes.push(FASL_TAG_GRAPH_REF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_GRAPH_REF);
            put_u32(&mut bytes, 0);

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("read cyclic pair graph");

            assert!(value.is_pair());
            let pair = value.downcast::<Pair>();
            assert!(Gc::ptr_eq(value.car().downcast::<Pair>(), pair));
            assert!(Gc::ptr_eq(value.cdr().downcast::<Pair>(), pair));
        });
    }

    #[test]
    fn legacy_ref_init_ref_roundtrips_shared_vector_element() {
        use super::{FASLReader, FASLWriter};
        use crate::runtime::{
            Scheme,
            value::{ByteVector, Value, Vector},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let shared = Value::new(ByteVector::from_slice(*ctx, &[1, 2, 3], true));
            let vector = Value::new(Vector::from_slice(*ctx, &[shared, shared]));
            let mut bytes = Vec::new();
            FASLWriter::new(ctx, &mut bytes)
                .write(vector)
                .expect("write legacy FASL refs");

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("read legacy FASL refs");
            let vector = value.downcast::<Vector>();

            assert_eq!(vector[0].get(), vector[1].get());
        });
    }

    #[test]
    fn fasl_writer_emits_loadable_zero_relocation_closure_with_code_block() {
        use super::{FASLWriter, FaslClosureSpec, FaslCodeBlockSpec};
        use crate::runtime::{
            Scheme,
            value::{Closure, CodeBlockKind, Value},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            FASLWriter::new(ctx, &mut bytes)
                .write_loaded_closure(&FaslClosureSpec {
                    code: FaslCodeBlockSpec {
                        bytes: &[0xc3],
                        entry_offset: 0,
                        arity: 0,
                        is_cont: false,
                        metadata: Value::new(false),
                        relocations: &[],
                    },
                    free: &[],
                    is_cont: false,
                })
                .expect("write unified FASL closure");

            assert_eq!(bytes[4], FASL_TAG_GROUP);
            assert_eq!(bytes[5], FASL_SITUATION_VISIT_REVISIT);
            assert_eq!(bytes[10], FASL_TAG_UNCOMPRESSED);
            assert_eq!(bytes[15], FASL_TAG_GRAPH);
            assert_eq!(bytes[24], FASL_TAG_CLOSURE);
            assert_eq!(bytes[25], FASL_TAG_GRAPH_DEF);

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load writer-produced closure");
            let closure = value.downcast::<Closure>();
            let code_block = closure.code_block;
            assert_eq!(closure.nfree, 0);
            assert_eq!(closure.code, code_block.entrypoint);
            assert!(matches!(code_block.kind, CodeBlockKind::Loaded));
            assert_eq!(code_block.arity.fixed_arity(), 0);
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_reads_zero_relocation_closure_with_code_block() {
        use super::{
            FASL_TAG_CLOSURE, FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF,
        };
        use crate::runtime::{
            Scheme,
            value::{Closure, CodeBlockKind},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 1); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_CLOSURE);
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 1); // code byte length
            bytes.push(0xc3); // ret
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes()); // arity
            bytes.push(0); // code-block is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 0); // relocation count
            put_u32(&mut bytes, 0); // closure free count
            bytes.push(0); // closure is_cont

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load zero-relocation code closure");
            let closure = value.downcast::<Closure>();
            let code_block = closure.code_block;
            assert_eq!(closure.nfree, 0);
            assert_eq!(closure.code, code_block.entrypoint);
            assert!(matches!(code_block.kind, CodeBlockKind::Loaded));
            assert_eq!(code_block.arity.fixed_arity(), 0);
            assert_ne!(code_block.loaded_code_handle, 0);
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_entry_resolves_graph_defined_code_block() {
        use super::{
            FASL_TAG_CLOSURE, FASL_TAG_CODE_BLOCK, FASL_TAG_ENTRY, FASL_TAG_F, FASL_TAG_GRAPH,
            FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
        };
        use crate::rsgc::Gc;
        use crate::runtime::{
            Scheme,
            value::{Closure, CodeBlockKind, Vector},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 1); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 2); // code block, closure
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 1); // code byte length
            bytes.push(0xc3); // ret
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes()); // arity
            bytes.push(0); // code-block is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 0); // relocation count
            bytes.push(FASL_TAG_CLOSURE);
            bytes.push(FASL_TAG_ENTRY);
            put_u32(&mut bytes, 0); // graph-defined code block
            put_u32(&mut bytes, 0); // closure free count
            bytes.push(0); // closure is_cont

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load closure using entry reference");
            let vector = value.downcast::<Vector>();
            let code_block = vector[0].get();
            let closure = vector[1].get().downcast::<Closure>();

            assert!(Gc::ptr_eq(closure.code_block, code_block.downcast()));
            assert_eq!(closure.code, closure.code_block.entrypoint);
            assert!(matches!(closure.code_block.kind, CodeBlockKind::Loaded));
        });
    }

    #[test]
    fn fasl_reader_reads_code_block_as_value() {
        use super::{FASL_TAG_CODE_BLOCK, FASL_TAG_F};
        use crate::runtime::{Scheme, value::CodeBlock};

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 1); // code byte length
            bytes.push(0xc3);
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 0); // relocation count

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("read code block through normal FASL reader");
            assert!(value.is::<CodeBlock>());
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_applies_runtime_data_code_block_relocation() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation},
        };
        use crate::runtime::{Scheme, symbols::RuntimeData, value::CodeBlock};

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 9); // ret plus one word patch slot
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::RuntimeData,
                target: FaslRelocTarget::RuntimeSymbol(RuntimeData::PairInfo.id()),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load runtime-data relocation");
            let code_block = value.downcast::<CodeBlock>();
            let patched = unsafe {
                std::ptr::read_unaligned((code_block.entrypoint.as_usize() + 1) as *const usize)
            };
            assert_eq!(patched, RuntimeData::PairInfo.address().as_usize());
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_applies_asmkit_abs8_code_block_relocation() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation, SideMetadataSlotKind},
        };
        use crate::runtime::{Scheme, value::CodeBlock};
        use asmkit::core::buffer::Reloc as AsmkitReloc;
        use mmtk::util::metadata::side_metadata::global_side_metadata_vm_base_address;

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 9); // ret plus one word patch slot
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::Asmkit(AsmkitReloc::Abs8),
                target: FaslRelocTarget::SideMetadata(SideMetadataSlotKind::Global),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load asmkit abs8 relocation");
            let code_block = value.downcast::<CodeBlock>();
            let patched = unsafe {
                std::ptr::read_unaligned((code_block.entrypoint.as_usize() + 1) as *const usize)
            };
            assert_eq!(patched, global_side_metadata_vm_base_address().as_usize());
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_applies_asmkit_x86_pc_rel4_code_entry_relocation() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation},
        };
        use crate::runtime::{
            Scheme,
            value::{CodeBlock, Vector},
        };
        use asmkit::core::buffer::Reloc as AsmkitReloc;

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 2); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 2);
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 1); // target code byte length
            bytes.push(0xc3);
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 0); // relocation count
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 1);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 5); // ret plus rel32 patch slot
            bytes.push(0xc3);
            bytes.extend_from_slice(&0i32.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::Asmkit(AsmkitReloc::X86PCRel4),
                target: FaslRelocTarget::Entry(0),
                addend: -4,
            }
            .encode(&mut bytes)
            .expect("encode relocation");

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load asmkit rel32 relocation");
            let values = value.downcast::<Vector>();
            let target = values[0].get().downcast::<CodeBlock>();
            let source = values[1].get().downcast::<CodeBlock>();
            let patched = unsafe {
                std::ptr::read_unaligned((source.entrypoint.as_usize() + 1) as *const i32)
            };
            let expected = target.entrypoint.as_usize() as i128
                - (source.entrypoint.as_usize() + 1 + 4) as i128;
            assert_eq!(patched, i32::try_from(expected).unwrap());
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_applies_side_metadata_code_block_relocation() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation, SideMetadataSlotKind},
        };
        use crate::runtime::{Scheme, value::CodeBlock};
        use mmtk::util::metadata::side_metadata::global_side_metadata_vm_base_address;

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 9); // ret plus one word patch slot
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::SideMetadata,
                target: FaslRelocTarget::SideMetadata(SideMetadataSlotKind::Global),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load side-metadata relocation");
            let code_block = value.downcast::<CodeBlock>();
            let patched = unsafe {
                std::ptr::read_unaligned((code_block.entrypoint.as_usize() + 1) as *const usize)
            };
            assert_eq!(patched, global_side_metadata_vm_base_address().as_usize());
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_applies_runtime_thunk_code_block_relocation() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation},
        };
        use crate::runtime::{Scheme, symbols::RuntimeThunk, value::CodeBlock};

        let thunk = RuntimeThunk::Thunk_wrong_number_of_args;
        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 9); // ret plus one word patch slot
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::RuntimeThunk,
                target: FaslRelocTarget::RuntimeSymbol(thunk.id()),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load runtime-thunk relocation");
            let code_block = value.downcast::<CodeBlock>();
            let patched = unsafe {
                std::ptr::read_unaligned((code_block.entrypoint.as_usize() + 1) as *const usize)
            };
            assert_eq!(patched, thunk.address().as_usize());
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_applies_data_slot_address_relocation_to_graph_object() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation},
        };
        use crate::runtime::{
            Scheme,
            value::{CodeBlock, Value, Vector},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 2); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 2);
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 0); // relocation target object
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 1);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 9); // ret plus one word patch slot
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::DataSlotAddress,
                target: FaslRelocTarget::Object(0),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load data-slot relocation");
            let values = value.downcast::<Vector>();
            let target = values[0].get();
            let code_block = values[1].get().downcast::<CodeBlock>();
            let slot_address = unsafe {
                std::ptr::read_unaligned((code_block.entrypoint.as_usize() + 1) as *const usize)
            };
            assert_eq!(slot_address, code_block.loaded_data_base.as_usize());
            let slot_value = unsafe { std::ptr::read(slot_address as *const Value) };
            assert_eq!(slot_value, target);
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_applies_data_slot_address_relocation_to_side_metadata_slot() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation, SideMetadataSlotKind},
        };
        use crate::runtime::{Scheme, value::CodeBlock};
        use mmtk::util::metadata::side_metadata::global_side_metadata_vm_base_address;

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 9); // ret plus one word patch slot
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::DataSlotAddress,
                target: FaslRelocTarget::SideMetadata(SideMetadataSlotKind::Global),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load side-metadata data-slot relocation");
            let code_block = value.downcast::<CodeBlock>();
            assert_eq!(code_block.loaded_data_slot_count, 1);
            assert_eq!(code_block.loaded_data_value_bitmap(), &[]);
            let slot_address = unsafe {
                std::ptr::read_unaligned((code_block.entrypoint.as_usize() + 1) as *const usize)
            };
            assert_eq!(slot_address, code_block.loaded_data_base.as_usize());
            let slot_word = unsafe { std::ptr::read(slot_address as *const usize) };
            assert_eq!(slot_word, global_side_metadata_vm_base_address().as_usize());
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_resolves_forward_code_entry_data_slot_address_relocation() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation},
        };
        use crate::runtime::{
            Scheme,
            value::{CodeBlock, Vector},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 2); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 2);
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 9); // ret plus one word patch slot
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::DataSlotAddress,
                target: FaslRelocTarget::Entry(1),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 1);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 1); // target code byte length
            bytes.push(0xc3);
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 0); // relocation count

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load forward code-entry data-slot relocation");
            let values = value.downcast::<Vector>();
            let source = values[0].get().downcast::<CodeBlock>();
            let target = values[1].get().downcast::<CodeBlock>();
            assert_eq!(source.loaded_data_value_bitmap(), &[]);
            let slot_address = unsafe {
                std::ptr::read_unaligned((source.entrypoint.as_usize() + 1) as *const usize)
            };
            let slot_word = unsafe { std::ptr::read(slot_address as *const usize) };
            assert_eq!(slot_word, target.entrypoint.as_usize());
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_keeps_raw_data_slots_out_of_value_bitmap() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation, SideMetadataSlotKind},
        };
        use crate::runtime::{
            Scheme,
            value::{CodeBlock, Vector},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 2); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 2);
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 0); // value relocation target
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 1);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 17); // ret plus two word patch slots
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 2); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::DataSlotAddress,
                target: FaslRelocTarget::Object(0),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode value relocation");
            FaslRelocation {
                offset: 1 + std::mem::size_of::<usize>() as u32,
                kind: FaslRelocKind::DataSlotAddress,
                target: FaslRelocTarget::SideMetadata(SideMetadataSlotKind::Global),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode raw relocation");

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load mixed data-slot relocations");
            let values = value.downcast::<Vector>();
            let code_block = values[1].get().downcast::<CodeBlock>();
            assert_eq!(code_block.loaded_data_slot_count, 2);
            assert_eq!(code_block.loaded_data_value_bitmap(), &[1]);
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_shares_data_slot_targets_across_code_blocks_in_graph() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation},
        };
        use crate::runtime::{
            Scheme,
            value::{CodeBlock, Vector},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 3); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 3);
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 0); // shared data target
            for code_index in [1, 2] {
                bytes.push(FASL_TAG_GRAPH_DEF);
                put_u32(&mut bytes, code_index);
                bytes.push(FASL_TAG_CODE_BLOCK);
                put_u32(&mut bytes, 9); // ret plus one word patch slot
                bytes.push(0xc3);
                bytes.extend_from_slice(&0usize.to_le_bytes());
                put_u32(&mut bytes, 0); // entry offset
                bytes.extend_from_slice(&0i32.to_le_bytes());
                bytes.push(0); // is_cont
                bytes.push(FASL_TAG_F); // metadata
                put_u32(&mut bytes, 1); // relocation count
                FaslRelocation {
                    offset: 1,
                    kind: FaslRelocKind::DataSlotAddress,
                    target: FaslRelocTarget::Object(0),
                    addend: 0,
                }
                .encode(&mut bytes)
                .expect("encode relocation");
            }

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load shared data-slot graph");
            let values = value.downcast::<Vector>();
            let first = values[1].get().downcast::<CodeBlock>();
            let second = values[2].get().downcast::<CodeBlock>();
            let first_slot = unsafe {
                std::ptr::read_unaligned((first.entrypoint.as_usize() + 1) as *const usize)
            };
            let second_slot = unsafe {
                std::ptr::read_unaligned((second.entrypoint.as_usize() + 1) as *const usize)
            };

            assert_eq!(first_slot, first.loaded_data_base.as_usize());
            assert_eq!(second_slot, first_slot);
            assert_eq!(second.loaded_data_slot_count, 0);
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_resolves_forward_data_slot_address_relocation() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation},
        };
        use crate::runtime::{
            Scheme,
            value::{CodeBlock, Value, Vector},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 2); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 2);
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 9); // ret plus one word patch slot
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::DataSlotAddress,
                target: FaslRelocTarget::Object(1),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 1);
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 0); // relocation target object

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load forward data-slot relocation");
            let values = value.downcast::<Vector>();
            let code_block = values[0].get().downcast::<CodeBlock>();
            let target = values[1].get();
            let slot_address = unsafe {
                std::ptr::read_unaligned((code_block.entrypoint.as_usize() + 1) as *const usize)
            };
            let slot_value = unsafe { std::ptr::read(slot_address as *const Value) };
            assert_eq!(slot_value, target);
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_applies_code_entry_relocation_to_graph_entry() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation},
        };
        use crate::runtime::{
            Scheme,
            value::{CodeBlock, Vector},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 2); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 2);
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 1); // target code byte length
            bytes.push(0xc3);
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 0); // relocation count
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 1);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 9); // ret plus one word patch slot
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::CodeEntry,
                target: FaslRelocTarget::Entry(0),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load code-entry relocation");
            let values = value.downcast::<Vector>();
            let target = values[0].get().downcast::<CodeBlock>();
            let source = values[1].get().downcast::<CodeBlock>();
            let patched = unsafe {
                std::ptr::read_unaligned((source.entrypoint.as_usize() + 1) as *const usize)
            };
            assert_eq!(patched, target.entrypoint.as_usize());
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_resolves_forward_code_entry_relocation() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation},
        };
        use crate::runtime::{
            Scheme,
            value::{CodeBlock, Vector},
        };

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, 2); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, 2);
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 0);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 9); // ret plus one word patch slot
            bytes.push(0xc3);
            bytes.extend_from_slice(&0usize.to_le_bytes());
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 1); // relocation count
            FaslRelocation {
                offset: 1,
                kind: FaslRelocKind::CodeEntry,
                target: FaslRelocTarget::Entry(1),
                addend: 0,
            }
            .encode(&mut bytes)
            .expect("encode relocation");
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, 1);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(&mut bytes, 1); // target code byte length
            bytes.push(0xc3);
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, 0); // relocation count

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load forward code-entry relocation");
            let values = value.downcast::<Vector>();
            let source = values[0].get().downcast::<CodeBlock>();
            let target = values[1].get().downcast::<CodeBlock>();
            let patched = unsafe {
                std::ptr::read_unaligned((source.entrypoint.as_usize() + 1) as *const usize)
            };
            assert_eq!(patched, target.entrypoint.as_usize());
        });
    }

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn fasl_reader_accepts_more_than_64_value_data_slots() {
        use super::{
            FASL_TAG_CODE_BLOCK, FASL_TAG_F, FASL_TAG_GRAPH, FASL_TAG_GRAPH_DEF, FASL_TAG_VECTOR,
            reloc::{FaslRelocKind, FaslRelocTarget, FaslRelocation},
        };
        use crate::runtime::{
            Scheme,
            value::{CodeBlock, Value, Vector},
        };

        const VALUE_SLOT_COUNT: usize = 65;

        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            put_u32(&mut bytes, 0); // lites
            bytes.push(FASL_TAG_GRAPH);
            put_u32(&mut bytes, (VALUE_SLOT_COUNT + 1) as u32); // graph length
            put_u32(&mut bytes, 0); // external count
            bytes.push(FASL_TAG_VECTOR);
            put_u32(&mut bytes, (VALUE_SLOT_COUNT + 1) as u32);
            for index in 0..VALUE_SLOT_COUNT {
                bytes.push(FASL_TAG_GRAPH_DEF);
                put_u32(&mut bytes, index as u32);
                bytes.push(FASL_TAG_VECTOR);
                put_u32(&mut bytes, 0);
            }
            bytes.push(FASL_TAG_GRAPH_DEF);
            put_u32(&mut bytes, VALUE_SLOT_COUNT as u32);
            bytes.push(FASL_TAG_CODE_BLOCK);
            put_u32(
                &mut bytes,
                (1 + VALUE_SLOT_COUNT * std::mem::size_of::<usize>()) as u32,
            );
            bytes.push(0xc3);
            bytes.resize(
                bytes.len() + VALUE_SLOT_COUNT * std::mem::size_of::<usize>(),
                0,
            );
            put_u32(&mut bytes, 0); // entry offset
            bytes.extend_from_slice(&0i32.to_le_bytes());
            bytes.push(0); // is_cont
            bytes.push(FASL_TAG_F); // metadata
            put_u32(&mut bytes, VALUE_SLOT_COUNT as u32);
            for index in 0..VALUE_SLOT_COUNT {
                FaslRelocation {
                    offset: 1 + (index * std::mem::size_of::<usize>()) as u32,
                    kind: FaslRelocKind::DataSlotAddress,
                    target: FaslRelocTarget::Object(index as u32),
                    addend: 0,
                }
                .encode(&mut bytes)
                .expect("encode relocation");
            }

            let value = FASLReader::new(ctx, std::io::Cursor::new(bytes))
                .read()
                .expect("load data-slot relocations");
            let values = value.downcast::<Vector>();
            let code_block = values[VALUE_SLOT_COUNT].get().downcast::<CodeBlock>();
            for index in [0, VALUE_SLOT_COUNT - 1] {
                let slot_address = unsafe {
                    std::ptr::read_unaligned(
                        (code_block.entrypoint.as_usize()
                            + 1
                            + index * std::mem::size_of::<usize>())
                            as *const usize,
                    )
                };
                let slot_value = unsafe { std::ptr::read(slot_address as *const Value) };
                assert_eq!(slot_value, values[index].get());
            }
            assert_eq!(code_block.loaded_data_slot_count as usize, VALUE_SLOT_COUNT);
        });
    }
}
