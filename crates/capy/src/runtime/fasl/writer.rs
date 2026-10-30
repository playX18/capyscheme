use std::{
    collections::BTreeMap,
    io::{self, BufWriter, Write},
};

use flate2::{Compression as FlateCompression, write::GzEncoder};
use im::HashSet;

use crate::heap::mmtk::util::Address;

use crate::runtime::{
    MutationContext,
    record::{RTD_OPAQUE, RTD_SEALED, Rcd, Rtd},
    value::{
        BigInt, ByteVector, Complex, HashTable, HashTableType, IntoValue, Keyword, Rational, Str,
        Symbol, Tuple, Value, Vector,
    },
    vm::syntax::Syntax,
};

use super::{
    ConstSpec, FASL_COMPRESSION_GZIP, FASL_COMPRESSION_NONE, FASL_MAGIC, FASL_TAG_BIGINT,
    FASL_TAG_BVECTOR, FASL_TAG_CHAR, FASL_TAG_COMPLEX, FASL_TAG_DLIST, FASL_TAG_F, FASL_TAG_FIXNUM,
    FASL_TAG_FLONUM, FASL_TAG_IMMEDIATE, FASL_TAG_KEYWORD, FASL_TAG_LOOKUP, FASL_TAG_NIL,
    FASL_TAG_PLIST, FASL_TAG_PROGRAM, FASL_TAG_RATIONAL, FASL_TAG_RCD, FASL_TAG_REF,
    FASL_TAG_REF_INIT, FASL_TAG_RTD, FASL_TAG_STR, FASL_TAG_SYMBOL, FASL_TAG_SYNTAX, FASL_TAG_T,
    FASL_TAG_TUPLE, FASL_TAG_UNINTERNED_SYMBOL, FASL_TAG_VECTOR, ProgramSpec, checked_u32_len,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Compression {
    None,
    Gzip,
}

impl Compression {
    fn tag(self) -> u8 {
        match self {
            Self::None => FASL_COMPRESSION_NONE,
            Self::Gzip => FASL_COMPRESSION_GZIP,
        }
    }
}

#[derive(Clone, Copy)]
pub enum Image<'a, 'gc> {
    Value(Value<'gc>),
    Program(&'a ProgramSpec<'a, 'gc>),
}

pub struct Writer<'gc, W: Write> {
    pub ctx: MutationContext<'gc>,
    pub writer: BufWriter<W>,
    pub lites: crate::heap::Gc<'gc, HashTable<'gc>>,
    pub stack: Vec<Value<'gc>>,
    pub reference_map: BTreeMap<Address, u32>,
    pub initmap: HashSet<Address>,
}

impl<'gc, W: Write> Writer<'gc, W> {
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

        if obj.is::<Rtd>() {
            let rtd = obj.downcast::<Rtd>();
            self.scan(rtd.name)?;
            self.scan(rtd.uid)?;
            self.scan(rtd.fields)?;
            if let Some(parent) = rtd.parent() {
                self.scan(Value::from(parent))?;
            }
            return Ok(());
        }

        if obj.is::<Rcd>() {
            let rcd = obj.downcast::<Rcd>();
            self.scan(Value::from(rcd.rtd))?;
            self.scan(rcd.prcd)?;
            self.scan(rcd.protocol)?;
            return Ok(());
        }

        log::debug!("{obj}");
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
            let id = self
                .lites
                .get(self.ctx, obj)
                .expect("index in range")
                .as_int32();
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

        if obj.is::<Rtd>() {
            let rtd = obj.downcast::<Rtd>();
            let parent = rtd.parent();
            let inherited_fields = parent.map_or(0, |p| p.field_count);
            let own_field_count =
                rtd.field_count
                    .checked_sub(inherited_fields)
                    .ok_or_else(|| {
                        io::Error::new(
                            io::ErrorKind::InvalidData,
                            "record type has fewer fields than its parent",
                        )
                    })?;
            self.put8(FASL_TAG_RTD)?;
            self.put(rtd.name)?;
            self.put(rtd.uid)?;
            self.put8((rtd.flags.get() & (RTD_SEALED | RTD_OPAQUE)) as u8)?;
            self.put32(own_field_count)?;
            self.put(rtd.fields)?;
            self.put(parent.map_or(Value::new(false), |p| Value::from(p)))?;
            return Ok(());
        }

        if obj.is::<Rcd>() {
            let rcd = obj.downcast::<Rcd>();
            self.put8(FASL_TAG_RCD)?;
            self.put(Value::from(rcd.rtd))?;
            self.put(rcd.prcd)?;
            self.put(rcd.protocol)?;
            return Ok(());
        }

        log::debug!("{obj}");
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
            let item = self.pop().expect("invariant holds");

            self.put(item)?;
        }

        Ok(())
    }

    pub fn put_header(&mut self) -> io::Result<()> {
        self.writer.write_all(FASL_MAGIC)
    }

    pub fn put_lites(&mut self) -> io::Result<()> {
        self.put32(self.lites.len() as u32)?;

        let mut entries: Vec<_> = self.lites.iter().collect();
        entries.sort_by_key(|(_, value)| value.as_int32());

        for (key, value) in entries {
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
                log::debug!("CAN'T SERIALIZE LITE: {key}");
                return Err(io::Error::new(
                    io::ErrorKind::Unsupported,
                    "Unsupported type for FASL serialization",
                ));
            }
        }

        Ok(())
    }

    pub fn write(self, obj: Value<'gc>) -> io::Result<()> {
        self.write_image(Image::Value(obj), Compression::None)
    }

    pub fn write_image(
        mut self,
        image: Image<'_, 'gc>,
        compression: Compression,
    ) -> io::Result<()> {
        match image {
            Image::Value(value) => self.scan(value)?,
            Image::Program(spec) => self.scan_program(spec)?,
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
        for code in spec.code_blocks {
            self.scan(code.metadata)?;
            for constant in code.constants {
                if let ConstSpec::Value(value) = constant {
                    self.scan(*value)?;
                }
            }
            for span in code.spans {
                self.scan(*span)?;
            }
        }
        Ok(())
    }

    fn put_image_payload(&mut self, image: Image<'_, 'gc>) -> io::Result<()> {
        match image {
            Image::Value(value) => self.put(value),
            Image::Program(spec) => self.put_program(spec),
        }
    }

    fn put_program(&mut self, spec: &ProgramSpec<'_, 'gc>) -> io::Result<()> {
        self.put8(FASL_TAG_PROGRAM)?;
        self.put64(crate::bytecode::def::format_hash())?;
        self.put32(checked_u32_len(spec.code_blocks.len())?)?;
        for code in spec.code_blocks {
            self.put32(code.arity as u32)?;
            self.put32(code.frame_size)?;
            self.put(code.metadata)?;
            self.put32(checked_u32_len(code.code.len())?)?;
            self.put_many(code.code)?;
            self.put32(checked_u32_len(code.pool.len())?)?;
            for word in code.pool {
                self.put32(*word)?;
            }
            self.put32(checked_u32_len(code.metadata_counts.len())?)?;
            for count in code.metadata_counts {
                self.put32(*count)?;
            }
            self.put32(checked_u32_len(code.constants.len())?)?;
            for constant in code.constants {
                match constant {
                    ConstSpec::Value(value) => {
                        self.put8(0)?;
                        self.put(*value)?;
                    }
                    ConstSpec::Code(index) => {
                        self.put8(1)?;
                        self.put32(*index)?;
                    }
                    ConstSpec::Cell(id) => {
                        self.put8(2)?;
                        self.put32(*id)?;
                    }
                }
            }
            self.put32(checked_u32_len(code.spans.len())?)?;
            for span in code.spans {
                self.put(*span)?;
            }
            self.put32(checked_u32_len(code.srcmap.len())?)?;
            let mut prev_start = 0u32;
            for run in code.srcmap {
                self.put_uleb128(run.start.wrapping_sub(prev_start))?;
                self.put_uleb128(run.len)?;
                self.put8(run.flags)?;
                self.put_uleb128(run.span)?;
                prev_start = run.start;
            }
        }
        self.put32(spec.entry)
    }

    fn put_uleb128(&mut self, mut value: u32) -> io::Result<()> {
        loop {
            let mut byte = (value & 0x7f) as u8;
            value >>= 7;
            if value != 0 {
                byte |= 0x80;
            }
            self.put8(byte)?;
            if value == 0 {
                return Ok(());
            }
        }
    }

    fn payload_writer<'a>(&self, writer: &'a mut Vec<u8>) -> Writer<'gc, &'a mut Vec<u8>> {
        Writer {
            ctx: self.ctx,
            writer: BufWriter::new(writer),
            lites: self.lites,
            stack: Vec::new(),
            reference_map: self.reference_map.clone(),
            initmap: HashSet::new(),
        }
    }

    fn put_payload(&mut self, compression: Compression, payload: &[u8]) -> io::Result<()> {
        self.put32(checked_u32_len(payload.len())?)?;
        match compression {
            Compression::None => {
                self.put32(checked_u32_len(payload.len())?)?;
                self.put_many(payload)
            }
            Compression::Gzip => {
                let mut encoder = GzEncoder::new(Vec::new(), FlateCompression::default());
                encoder.write_all(payload)?;
                let compressed = encoder.finish()?;
                self.put32(checked_u32_len(compressed.len())?)?;
                self.put_many(compressed)
            }
        }
    }

    pub fn new(ctx: MutationContext<'gc>, writer: W) -> Self {
        Self {
            ctx,
            writer: BufWriter::new(writer),
            lites: HashTable::new(ctx, HashTableType::Eq, 32, 0.75),
            stack: Vec::new(),
            reference_map: BTreeMap::new(),
            initmap: HashSet::new(),
        }
    }
}
