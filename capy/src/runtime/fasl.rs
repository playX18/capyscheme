//! FASL (Fast Loading) format
//!
//! Serialzies/deserializes Scheme values to/from a binary format
//! that can be loaded quickly at runtime.

use std::io::{self, BufReader, BufWriter, Read, Write};

use rsgc::Gc;

use crate::runtime::{
    Context,
    value::{
        BigInt, ByteVector, Complex, HashTable, HashTableType, IntoValue, Rational, Str, Symbol,
        Tuple, Value, Vector,
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

pub struct FASLWriter<'gc, W: Write> {
    pub ctx: Context<'gc>,
    pub writer: BufWriter<W>,
    pub lites: Gc<'gc, HashTable<'gc>>,
    pub stack: Vec<Value<'gc>>,
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
        if obj.is_immediate() {
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

        if obj.is_number() {
            return Ok(());
        }

        return Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "Unsupported type for FASL serialization",
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
            return Ok(());
        }
        println!("CAN'T SERIALIZE: {obj}");
        return Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "Unsupported type for FASL serialization",
        ));
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

                self.put32(sym.len() as u32)?;
                self.put_many(sym.to_string().as_bytes())?;
            } else if key.is::<Str>() {
                let str = key.downcast::<Str>();
                self.put8(FASL_TAG_STR)?;
                self.put32(str.len() as u32)?;
                self.put_many(str.to_string().as_bytes())?;
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
        self.put(obj)?;
        self.writer.flush()?;
        Ok(())
    }

    pub fn new(ctx: Context<'gc>, writer: W) -> Self {
        Self {
            ctx,
            writer: BufWriter::new(writer),
            lites: HashTable::new(&ctx, HashTableType::Eq, 32, 0.75),
            stack: Vec::new(),
        }
    }
}

pub struct FASLReader<'gc, R: io::Read> {
    pub ctx: Context<'gc>,
    pub reader: BufReader<R>,
    pub lites: Gc<'gc, HashTable<'gc>>,
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
                    let str = std::str::from_utf8(&buf)
                        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Invalid UTF-8"))?;

                    let sym = Value::new(Symbol::from_str(self.ctx, str));

                    sym
                }
                FASL_TAG_UNINTERNED_SYMBOL => {
                    let len = self.read32()? as usize;
                    let mut buf = vec![0; len];
                    self.reader.read_exact(&mut buf)?;
                    let str = std::str::from_utf8(&buf)
                        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Invalid UTF-8"))?;

                    Value::new(Symbol::from_str_uninterned(&self.ctx, str, None))
                }
                FASL_TAG_STR => {
                    let len = self.read32()? as usize;
                    let mut buf = vec![0u8; len];
                    self.reader.read_exact(&mut buf)?;
                    let str = std::str::from_utf8(&buf)
                        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Invalid UTF-8"))?;
                    Value::new(Str::new(&self.ctx, str, false))
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

        match tag {
            _x @ FASL_TAG_FIXNUM => {
                let value = self.read32()? as i32;
                Ok(Value::new(value))
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
                Ok(Value::new(Vector::from_slice(&self.ctx, &vec)))
            }

            _x @ FASL_TAG_TUPLE => {
                let count = self.read32()? as usize;
                let mut vec = Vec::with_capacity(count);

                for _ in 0..count {
                    vec.push(self.read_value()?);
                }
                Ok(Value::new(Tuple::from_slice(&self.ctx, &vec)))
            }

            _x @ FASL_TAG_BVECTOR => {
                let count = self.read64()? as usize;
                let mut buf = vec![0u8; count];
                self.reader.read_exact(&mut buf)?;
                Ok(Value::new(ByteVector::from_slice(&self.ctx, &buf, true)))
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
                let real = self.read_value()?.number().ok_or_else(|| {
                    io::Error::new(
                        io::ErrorKind::InvalidData,
                        "Expected a number for real part",
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
                Ok(Value::new(Syntax::new(
                    self.ctx, expr, wrap, module, source,
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
            lites: HashTable::new(&ctx, HashTableType::Eq, 32, 0.75),
        }
    }
}
