use std::{
    collections::BTreeMap,
    io::{self, BufWriter, Write},
};

use flate2::{Compression, write::GzEncoder};
use im::HashSet;

use crate::rsgc::mmtk::util::Address;

use crate::runtime::{
    Context,
    value::{
        BigInt, ByteVector, Complex, HashTable, HashTableType, IntoValue, Keyword, Rational, Str,
        Symbol, Tuple, Value, Vector,
    },
    vm::syntax::Syntax,
};

use super::{
    CodeSpec, FASL_COMPRESSION_GZIP, FASL_COMPRESSION_NONE, FASL_MAGIC, FASL_TAG_BEGIN,
    FASL_TAG_BIGINT, FASL_TAG_BVECTOR, FASL_TAG_CHAR, FASL_TAG_CLOSURE, FASL_TAG_COMPLEX,
    FASL_TAG_DLIST, FASL_TAG_ENTRY, FASL_TAG_F, FASL_TAG_FIXNUM, FASL_TAG_FLONUM, FASL_TAG_GRAPH,
    FASL_TAG_GRAPH_DEF, FASL_TAG_IMMEDIATE, FASL_TAG_KEYWORD, FASL_TAG_LOOKUP, FASL_TAG_NIL,
    FASL_TAG_PLIST, FASL_TAG_RATIONAL, FASL_TAG_REF, FASL_TAG_REF_INIT, FASL_TAG_STR,
    FASL_TAG_SYMBOL, FASL_TAG_SYNTAX, FASL_TAG_T, FASL_TAG_TUPLE, FASL_TAG_UNINTERNED_SYMBOL,
    FASL_TAG_UNLINKED_CODEBLOCK, FASL_TAG_VECTOR, FASL_VERSION, ProgramSpec, checked_u32_len,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FaslCompression {
    None,
    Gzip,
}

impl FaslCompression {
    fn tag(self) -> u8 {
        match self {
            Self::None => FASL_COMPRESSION_NONE,
            Self::Gzip => FASL_COMPRESSION_GZIP,
        }
    }
}

#[derive(Clone, Copy)]
pub enum FaslImage<'a, 'gc> {
    Value(Value<'gc>),
    Program(&'a ProgramSpec<'a, 'gc>),
}

pub struct FaslWriter<'gc, W: Write> {
    pub ctx: Context<'gc>,
    pub writer: BufWriter<W>,
    pub lites: crate::rsgc::Gc<'gc, HashTable<'gc>>,
    pub stack: Vec<Value<'gc>>,
    pub reference_map: BTreeMap<Address, u32>,
    pub initmap: HashSet<Address>,
}

impl<'gc, W: Write> FaslWriter<'gc, W> {
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

        if self
            .reference_map
            .contains_key(&obj.as_cell_raw().to_address())
        {
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
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "Unsupported type for FASL serialization",
        ))
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
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            format!("Unsupported type for FASL serialization: {}", obj),
        ))
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

    pub fn put_header(&mut self) -> io::Result<()> {
        self.writer.write_all(FASL_MAGIC)?;
        self.put32(FASL_VERSION)
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

    pub fn write(self, obj: Value<'gc>) -> io::Result<()> {
        self.write_image(FaslImage::Value(obj), FaslCompression::None)
    }

    pub fn write_image(
        mut self,
        image: FaslImage<'_, 'gc>,
        compression: FaslCompression,
    ) -> io::Result<()> {
        match image {
            FaslImage::Value(value) => self.scan(value)?,
            FaslImage::Program(spec) => self.scan_program(spec)?,
        }
        self.put_header()?;
        self.put8(compression.tag())?;
        self.put_lites()?;
        let mut payload = Vec::new();
        {
            let mut writer = self.payload_writer(&mut payload);
            writer.put_image_payload(image)?;
            writer.writer.flush()?;
        }
        self.put_payload(compression, &payload)?;
        self.writer.flush()?;
        Ok(())
    }

    fn scan_program(&mut self, spec: &ProgramSpec<'_, 'gc>) -> io::Result<()> {
        for value in spec.values {
            self.scan(value.value)?;
        }
        for code in spec.code_blocks {
            self.scan(code.code.metadata)?;
        }
        Ok(())
    }

    fn put_image_payload(&mut self, image: FaslImage<'_, 'gc>) -> io::Result<()> {
        match image {
            FaslImage::Value(value) => self.put(value),
            FaslImage::Program(spec) => self.put_program(spec),
        }
    }

    fn put_program(&mut self, spec: &ProgramSpec<'_, 'gc>) -> io::Result<()> {
        self.put8(FASL_TAG_GRAPH)?;
        self.put32(spec.graph_len)?;
        self.put32(0)?;
        let item_count = checked_u32_len(spec.values.len() + spec.code_blocks.len() + 1)?;
        self.put8(FASL_TAG_BEGIN)?;
        self.put32(item_count)?;
        for value in spec.values {
            self.put8(FASL_TAG_GRAPH_DEF)?;
            self.put32(value.index)?;
            self.put(value.value)?;
        }
        for code in spec.code_blocks {
            self.put8(FASL_TAG_GRAPH_DEF)?;
            self.put32(code.index)?;
            self.put_code_block(&code.code)?;
        }
        self.put_entry_closure(spec.entry_code_index, spec.entry_is_cont)?;
        Ok(())
    }

    fn payload_writer<'a>(&self, writer: &'a mut Vec<u8>) -> FaslWriter<'gc, &'a mut Vec<u8>> {
        FaslWriter {
            ctx: self.ctx,
            writer: BufWriter::new(writer),
            lites: self.lites,
            stack: Vec::new(),
            reference_map: self.reference_map.clone(),
            initmap: HashSet::new(),
        }
    }

    fn put_payload(&mut self, compression: FaslCompression, payload: &[u8]) -> io::Result<()> {
        self.put32(checked_u32_len(payload.len())?)?;
        match compression {
            FaslCompression::None => {
                self.put32(checked_u32_len(payload.len())?)?;
                self.put_many(payload)
            }
            FaslCompression::Gzip => {
                let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
                encoder.write_all(payload)?;
                let compressed = encoder.finish()?;
                self.put32(checked_u32_len(compressed.len())?)?;
                self.put_many(compressed)
            }
        }
    }

    fn put_entry_closure(&mut self, entry_code_index: u32, is_cont: bool) -> io::Result<()> {
        self.put8(FASL_TAG_CLOSURE)?;
        self.put8(FASL_TAG_ENTRY)?;
        self.put32(entry_code_index)?;
        self.put32(0)?;
        self.put8(u8::from(is_cont))
    }

    fn put_code_block(&mut self, spec: &CodeSpec<'_, 'gc>) -> io::Result<()> {
        self.put8(FASL_TAG_UNLINKED_CODEBLOCK)?;
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
