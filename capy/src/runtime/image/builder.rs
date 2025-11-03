use rsgc::{Gc, mmtk::util::Address};
use std::{
    collections::BTreeMap,
    io::{Read, Write},
};

use crate::{
    prelude::{
        Boxed, ByteVector, ClosureRef, HashTable, HashTableType, IntoValue, Module, Number, Str,
        Stringbuf, Symbol, Tagged, Tuple, Value, Vector, WeakMapping, WeakSet, WeakTable,
    },
    runtime::{
        Context,
        fluids::{DynamicStateObject, Fluid},
        modules::Variable,
        vm::{
            libraries::LIBRARY_COLLECTION,
            syntax::{Syntax, SyntaxTransformer},
        },
    },
};

pub const TYPE_IMMEDIATE: u8 = 0;
pub const TYPE_REF: u8 = 1;
pub const TYPE_TRUE: u8 = 2;
pub const TYPE_FALSE: u8 = 3;
pub const TYPE_I32: u8 = 4;
pub const TYPE_CHAR: u8 = 5;
pub const TYPE_EOF: u8 = 6;
pub const TYPE_NULL: u8 = 7;
pub const TYPE_UNDEFINED: u8 = 8;
pub const TYPE_BWP: u8 = 9;

/// An image builder for serializing Scheme system state into
/// a binary image.
///
/// This type can only serialize a system state with *one* thread active.
pub struct ImageBuilder<'gc> {
    pub ctx: Context<'gc>,
    pub output: Vec<u8>,
    /// A map from object to its reference index in the image.
    pub reference_map: BTreeMap<Address, u32>,
    pub to_serialize: Vec<Value<'gc>>,
    pub fbase_to_lib: BTreeMap<Address, usize>,
}

impl<'gc> ImageBuilder<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self {
            ctx,
            output: Vec::with_capacity(4096),
            reference_map: BTreeMap::new(),
            to_serialize: Vec::new(),
            fbase_to_lib: BTreeMap::new(),
        }
    }

    pub fn serialize(mut self) -> std::io::Result<Vec<u8>> {
        self.serialize_libraries()?;

        Ok(self.output)
    }

    /// Walk all the loaded dynamic libraries and serialize them into the image. It performs ZSTD
    /// compression on each library ELF binary.
    fn serialize_libraries(&mut self) -> std::io::Result<()> {
        let libs = LIBRARY_COLLECTION.fetch(&self.ctx);

        let mut libraries = Vec::new();

        libs.for_each_library(|lib| {
            let file = std::fs::File::open(&lib.path).expect("Failed to open library file");
            let mut reader = std::io::BufReader::new(file);
            let mut buffer = Vec::new();
            reader
                .read_to_end(&mut buffer)
                .expect("Failed to read library file");
            let compressed = zstd::encode_all(&buffer[..], 3).expect("Failed to compress library");
            libraries.push((lib.fbase, compressed));
        });

        let mut index = 0;

        for (fbase, bin) in libraries {
            let pos = self.output.len();
            let size = bin.len() as u64;
            self.write64(size)?;
            self.write_many(&bin)?;
            self.fbase_to_lib.insert(fbase, index);
            index += 1;
        }

        Ok(())
    }

    pub fn write_many(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        let pos = self.output.len();
        self.output.write_all(bytes)?;
        Ok(pos)
    }

    pub fn write8(&mut self, byte: u8) -> std::io::Result<usize> {
        let pos = self.output.len();
        self.output.write_all(&[byte])?;
        Ok(pos)
    }

    pub fn write16(&mut self, value: u16) -> std::io::Result<usize> {
        let pos = self.output.len();
        self.output.write_all(&value.to_le_bytes())?;
        Ok(pos)
    }

    pub fn write32(&mut self, value: u32) -> std::io::Result<usize> {
        let pos = self.output.len();
        self.output.write_all(&value.to_le_bytes())?;
        Ok(pos)
    }

    pub fn write64(&mut self, value: u64) -> std::io::Result<usize> {
        let pos = self.output.len();
        self.output.write_all(&value.to_le_bytes())?;
        Ok(pos)
    }

    pub fn patch8(&mut self, position: usize, byte: u8) -> std::io::Result<()> {
        self.output[position] = byte;
        Ok(())
    }

    pub fn patch16(&mut self, position: usize, value: u16) -> std::io::Result<()> {
        let bytes = value.to_le_bytes();
        self.output[position..position + 2].copy_from_slice(&bytes);
        Ok(())
    }

    pub fn patch32(&mut self, position: usize, value: u32) -> std::io::Result<()> {
        let bytes = value.to_le_bytes();
        self.output[position..position + 4].copy_from_slice(&bytes);
        Ok(())
    }

    pub fn patch64(&mut self, position: usize, value: u64) -> std::io::Result<()> {
        let bytes = value.to_le_bytes();
        self.output[position..position + 8].copy_from_slice(&bytes);
        Ok(())
    }

    pub fn write_reference<T>(&mut self, object: Gc<'gc, T>) -> std::io::Result<usize>
    where
        T: Tagged,
    {
        let addr = object.as_gcobj().to_address();
        let index = match self.reference_map.get(&addr) {
            Some(&index) => index,
            None => {
                let index = self.reference_map.len() as u32;
                self.reference_map.insert(addr, index);
                self.to_serialize.push(object.into());
                index
            }
        };
        self.write32(index)
    }

    /// Serialize a Scheme value into the image.
    ///
    // TODO: Maybe serialize in NaN-boxed format directly?
    pub fn write_value(&mut self, val: Value<'gc>) -> std::io::Result<usize> {
        if val.is_immediate() {
            if val.is_bool() {
                if val.as_bool() {
                    self.write8(TYPE_TRUE)
                } else {
                    self.write8(TYPE_FALSE)
                }
            } else if val.is_undefined_or_null() {
                if val == Value::undefined() {
                    self.write8(TYPE_UNDEFINED)
                } else {
                    self.write8(TYPE_NULL)
                }
            } else if val.is_eof() {
                self.write8(TYPE_EOF)
            } else if val.is_char() {
                self.write8(TYPE_CHAR)?;
                self.write32(val.char() as u32)
            } else if val.is_int32() {
                self.write8(TYPE_I32)?;
                self.write32(val.as_int32() as u32)
            } else if val.is_bwp() {
                self.write8(TYPE_BWP)
            } else {
                self.write8(TYPE_IMMEDIATE)?;
                self.write64(val.bits() as u64)
            }
        } else {
            self.write8(TYPE_REF)?;
            let addr = unsafe { Address::from_usize(val.bits() as _) };
            let index = match self.reference_map.get(&addr) {
                Some(&index) => index,
                None => {
                    let index = self.reference_map.len() as u32;
                    self.reference_map.insert(addr, index);
                    self.to_serialize.push(val);
                    index
                }
            };

            self.write32(index)
        }
    }

    /* serialization for all types with `Tagged` implemented on them */

    pub fn write_pair(&mut self, pair: Value<'gc>) -> std::io::Result<()> {
        let car = pair.car();
        let cdr = pair.cdr();
        self.write_value(car)?;
        self.write_value(cdr)?;
        Ok(())
    }

    pub fn write_vector(&mut self, vector: Gc<'gc, Vector<'gc>>) -> std::io::Result<()> {
        let len = vector.len();
        self.write32(len as u32)?;
        for i in 0..len {
            let elem = vector[i].get();
            self.write_value(elem)?;
        }
        Ok(())
    }

    pub fn write_tuple(&mut self, tuple: Gc<'gc, Tuple<'gc>>) -> std::io::Result<()> {
        let len = tuple.len();
        self.write32(len as u32)?;
        for i in 0..len {
            let elem = tuple[i].get();
            self.write_value(elem)?;
        }
        Ok(())
    }

    pub fn write_bytevector(&mut self, bytevector: Gc<'gc, ByteVector>) -> std::io::Result<()> {
        if bytevector.is_mapping() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Cannot serialize mapped ByteVector",
            ));
        }

        let len = bytevector.len();
        self.write64(len as u64)?;
        let bytes = bytevector.as_slice();
        self.output.write_all(bytes)
    }

    pub(crate) fn write_stringbuf(&mut self, string: Gc<'gc, Stringbuf>) -> std::io::Result<()> {
        let len = string.len();
        self.write32(len as u32)?;
        self.write8(string.is_mutable() as _)?;
        self.write8(string.is_wide() as _)?;

        if string.is_wide() {
            for ch in string.wide_chars() {
                self.write32(*ch as u32)?;
            }
        } else {
            for ch in string.chars() {
                self.write8(*ch)?;
            }
        }

        Ok(())
    }

    pub fn write_string(&mut self, string: Gc<'gc, Str<'gc>>) -> std::io::Result<()> {
        let length = string.len();
        let start = string.start.get();
        let buf = string.stringbuf.get();

        self.write32(start as u32)?;
        self.write32(length as u32)?;
        self.write_reference(buf)?;
        Ok(())
    }

    pub fn write_symbol(&mut self, symbol: Gc<'gc, Symbol<'gc>>) -> std::io::Result<()> {
        let strbuf = symbol.stringbuf;
        self.write_reference(strbuf)?;
        Ok(())
    }

    pub fn write_number(&mut self, n: Number<'gc>) -> std::io::Result<()> {
        match n {
            Number::Fixnum(_) | Number::Flonum(_) => {
                unreachable!()
            }

            Number::BigInt(bn) => {
                self.write32(bn.len() as u32)?;
                for limb in bn.iter() {
                    self.write64(*limb)?;
                }

                Ok(())
            }

            Number::Complex(cn) => {
                self.write_value(cn.real.into_value(self.ctx))?;
                self.write_value(cn.imag.into_value(self.ctx))?;
                Ok(())
            }

            Number::Rational(rn) => {
                self.write_value(rn.numerator.into_value(self.ctx))?;
                self.write_value(rn.denominator.into_value(self.ctx))?;
                Ok(())
            }
        }
    }

    pub fn write_box(&mut self, b: Gc<'gc, Boxed<'gc>>) -> std::io::Result<()> {
        let value = b.val;
        self.write_value(value)?;
        Ok(())
    }

    pub fn write_module(&mut self, module: Gc<'gc, Module<'gc>>) -> std::io::Result<()> {
        let obarray = module.obarray.get();
        let uses = module.uses.get();
        let binder = module.binder;
        let declarative = module.declarative.get();
        let transformer = module.transformer;
        let name = module.name.get();
        let version = module.version.get();
        let kind = module.kind.get();
        let import_obarray = module.import_obarray;
        let submodules = module.submodules;
        let filename = module.filename.get();
        let public_interface = module.public_interface.get();
        let next_unique_id = module
            .next_unique_id
            .load(std::sync::atomic::Ordering::Relaxed);
        let replacements = module.replacements.get();
        let inlinable_exports = module.inlinable_exports.get();
        let environment = module.environment.get();

        self.write_reference(obarray)?;
        self.write_value(uses)?;
        self.write_value(binder)?;
        self.write8(declarative as u8)?;
        self.write_value(transformer)?;
        self.write_value(name)?;
        self.write_value(version)?;
        self.write8(kind as u8)?;
        self.write_reference(import_obarray)?;
        self.write_reference(submodules)?;
        self.write_value(filename)?;
        if let Some(iface) = public_interface {
            self.write8(1)?;
            self.write_reference(iface)?;
        } else {
            self.write8(0)?;
        }

        self.write64(next_unique_id as _)?;
        self.write_value(replacements)?;
        self.write_value(inlinable_exports)?;
        self.write_value(environment)?;
        Ok(())
    }

    pub fn write_variable(&mut self, var: Gc<'gc, Variable<'gc>>) -> std::io::Result<()> {
        self.write_value(var.value.get())?;
        Ok(())
    }

    pub fn write_hashtable(&mut self, ht: Gc<'gc, HashTable<'gc>>) -> std::io::Result<()> {
        let typ = ht.typ();
        match typ {
            HashTableType::Eq => {
                self.write8(0)?;
            }
            HashTableType::Eqv => {
                self.write8(1)?;
            }
            HashTableType::Equal => {
                self.write8(2)?;
            }
            HashTableType::String => {
                self.write8(3)?;
            }
            HashTableType::Generic(handler) => {
                self.write8(4)?;
                self.write_value(handler)?;
            }
        }

        let count = ht.len();
        self.write32(count as u32)?;
        for (key, val) in ht.iter() {
            self.write_value(key)?;
            self.write_value(val)?;
        }

        Ok(())
    }

    pub fn write_weaktable(&mut self, ht: Gc<'gc, WeakTable<'gc>>) -> std::io::Result<()> {
        let mut kvs = Vec::with_capacity(ht.len());
        // collect entries first
        ht.for_each(self.ctx, |key, val| {
            kvs.push((key, val));
        });
        // write count of collected entries since `ht.len()` may be inaccurate and `for_each` may
        // vacuum dead entries.
        let count = kvs.len();
        self.write32(count as u32)?;
        for (key, val) in kvs {
            self.write_value(key)?;
            self.write_value(val)?;
        }

        Ok(())
    }

    pub fn write_weakmapping(&mut self, wmap: Gc<'gc, WeakMapping<'gc>>) -> std::io::Result<()> {
        let key = wmap.key(&self.ctx);
        let value = wmap.value(&self.ctx);

        self.write_value(key)?;
        self.write_value(value)?;

        Ok(())
    }

    pub fn write_weakset(&mut self, wset: Gc<'gc, WeakSet<'gc>>) -> std::io::Result<()> {
        let mut elements = Vec::with_capacity(4);
        // collect elements first
        wset.for_each(&self.ctx, |elem| {
            elements.push(elem);
        });
        // write count of collected elements since `wset.len()` may be inaccurate and `for_each` may
        // vacuum dead entries.
        let count = elements.len();
        self.write32(count as u32)?;
        for elem in elements {
            self.write_value(elem)?;
        }

        Ok(())
    }

    pub fn write_syntax(&mut self, syntax: Gc<'gc, Syntax<'gc>>) -> std::io::Result<()> {
        self.write_value(syntax.expr)?;
        self.write_value(syntax.wrap)?;
        self.write_value(syntax.module)?;
        self.write_value(syntax.source)?;

        Ok(())
    }

    pub fn write_syntax_transformer(
        &mut self,
        transformer: Gc<'gc, SyntaxTransformer<'gc>>,
    ) -> std::io::Result<()> {
        let name = transformer.name();
        let typ = transformer.typ();
        let binding = transformer.binding();

        self.write_value(name)?;
        self.write_value(typ)?;
        self.write_value(binding)?;

        Ok(())
    }

    pub fn write_fluid(&mut self, fluid: Gc<'gc, Fluid<'gc>>) -> std::io::Result<()> {
        let value = fluid.value;
        self.write_value(value)?;
        Ok(())
    }

    pub fn write_dynamic_state_object(
        &mut self,
        obj: Gc<'gc, DynamicStateObject>,
    ) -> std::io::Result<()> {
        let state = obj.saved;
        self.write_value(state)?;
        Ok(())
    }

    /// The *most* complex object to serialize: closures.
    ///
    /// Requires finding out dynamic library its code comes from, or native function
    /// it belongs to (if any), and then serializing this reference accordingly.
    pub fn write_closure(&mut self, closure: ClosureRef<'gc>) -> std::io::Result<()> {
        Ok(())
    }
}
