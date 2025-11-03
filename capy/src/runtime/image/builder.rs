use rsgc::{
    Gc,
    mmtk::util::Address,
    object::{GCObject, HeapObjectHeader},
};
use std::{
    collections::BTreeMap,
    io::{Read, Write},
};

use crate::{
    prelude::{
        Boxed, ByteVector, Closure, ClosureRef, HashTable, HashTableType, IntoValue, Module,
        NativeProc, Number, ScmHeader, Str, Stringbuf, Symbol, Tagged, Tuple, Value, Vector,
        WeakMapping, WeakSet, WeakTable, symbol_table,
    },
    runtime::{
        Context,
        fluids::{DynamicStateObject, Fluid},
        modules::Variable,
        vm::{
            control::ContinuationMarks,
            ffi::Pointer,
            libraries::LIBRARY_COLLECTION,
            syntax::{Syntax, SyntaxTransformer},
            trampolines::{get_cont_trampoline_from_scheme, get_trampoline_from_scheme},
        },
    },
    utils::formatted_size,
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

/// Closure from dynamic library: dlsym from library[ix] and sname.
pub const TYPE_CLOSURE_DYNLIB: u8 = 20;
/// Native closure: read from vec of native function pointers
pub const TYPE_CLOSURE_NATIVE: u8 = 21;

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
    pub native_procedures: Vec<Address>,
}

impl<'gc> ImageBuilder<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self {
            ctx,
            output: Vec::with_capacity(8 * 1024 * 1024),
            reference_map: BTreeMap::new(),
            to_serialize: Vec::new(),
            fbase_to_lib: BTreeMap::new(),
            native_procedures: Vec::new(),
        }
    }

    pub fn serialize(mut self) -> std::io::Result<Vec<u8>> {
        let mut scan = ReferenceMapBuilder::new(self.ctx);
        scan.scan_context();
        scan.run_to_end();

        self.reference_map = scan.reference_map;

        println!("Total objects to serialize: {}", self.reference_map.len());

        self.native_procedures = super::all_native_procedures(self.ctx);
        println!("total native procedures: {}", self.native_procedures.len());
        self.serialize_libraries()?;
        let serialize_libs_end = self.output.len();

        let heap_start = self.output.len();
        self.serialize_reference_map()?;
        let heap_end = self.output.len();
        println!(
            "Serialized heap: {} (libraries: {}), total: {}",
            formatted_size(heap_end - heap_start),
            formatted_size(serialize_libs_end),
            formatted_size(self.output.len())
        );

        {
            self.write_symbol_table()?;
            self.write_globals();

            let dynstate = self.ctx.dynamic_state();
            self.write_value(dynstate)?;
            /*let marks = self.ctx.current_continuation_marks();
            self.write_value(marks.into())?;*/
        }

        Ok(self.output)
    }

    fn serialize_reference_map(&mut self) -> std::io::Result<()> {
        self.write32(self.reference_map.len() as u32)?;
        let objects = self.reference_map.clone();
        let mut objects_vec = objects
            .iter()
            .map(|x| (x.0.clone(), x.1.clone()))
            .collect::<Vec<_>>();
        objects_vec.sort_by(|a, b| a.1.cmp(&b.1));
        for (addr, index) in objects_vec.into_iter() {
            let gc_object = GCObject::from_address(addr);

            let ser_start = self.output.len();
            self.write32(index)?;
            self.write32(0)?;
            self.write32((gc_object.instance_size() - size_of::<HeapObjectHeader>()) as u32)?;
            unsafe {
                // write header for the object
                self.write64(addr.as_ref::<ScmHeader>().word)?;
            }

            let val = Value::from_raw(addr.as_usize() as _);

            if val.is_pair() {
                self.write_pair(val)?;
            } else if let Some(val) = val.try_as::<DynamicStateObject>() {
                self.write_dynamic_state_object(val)?;
            } else if let Some(val) = val.try_as::<Fluid>() {
                self.write_fluid(val)?;
            } else if let Some(val) = val.try_as::<Module>() {
                self.write_module(val)?;
            } else if let Some(val) = val.try_as::<Variable>() {
                self.write_variable(val)?;
            } else if let Some(val) = val.try_as::<Boxed>() {
                self.write_box(val)?;
            } else if let Some(val) = val.try_as::<HashTable>() {
                self.write_hashtable(val)?;
            } else if let Some(num) = val.number() {
                self.write_number(num)?;
            } else if let Some(val) = val.try_as::<NativeProc>() {
                self.write_native_proc(val)?;
            } else if let Some(val) = val.try_as::<Closure>() {
                self.write_closure(val)?;
            } else if let Some(val) = val.try_as::<Stringbuf>() {
                self.write_stringbuf(val)?;
            } else if let Some(val) = val.try_as::<Str>() {
                self.write_string(val)?;
            } else if let Some(val) = val.try_as::<Symbol>() {
                self.write_symbol(val)?;
            } else if let Some(val) = val.try_as::<Vector>() {
                self.write_vector(val)?;
            } else if let Some(val) = val.try_as::<ByteVector>() {
                self.write_bytevector(val)?;
            } else if let Some(val) = val.try_as::<Tuple>() {
                self.write_tuple(val)?;
            } else if let Some(val) = val.try_as::<WeakSet>() {
                self.write_weakset(val)?;
            } else if let Some(val) = val.try_as::<WeakMapping>() {
                self.write_weakmapping(val)?;
            } else if let Some(val) = val.try_as::<WeakTable>() {
                self.write_weaktable(val)?;
            } else if let Some(cmarks) = val.try_as::<ContinuationMarks>() {
                self.write_cmarks(cmarks)?;
            } else if let Some(syntax) = val.try_as::<Syntax>() {
                self.write_syntax(syntax)?;
            } else if let Some(transformer) = val.try_as::<SyntaxTransformer>() {
                self.write_syntax_transformer(transformer)?;
            } else if let Some(ptr) = val.try_as::<Pointer>() {
                self.write_pointer(ptr)?;
            } else {
                panic!(
                    "Unknown object type during serialization: {val:?} {:?}",
                    val.typ8().bits()
                );
            }

            let ser_end = self.output.len();
            self.patch32(ser_start, (ser_end - ser_start) as u32)?;
        }

        Ok(())
    }

    fn write_symbol_table(&mut self) -> std::io::Result<()> {
        let sym_table = symbol_table(&self.ctx);
        self.write_reference(sym_table)?;
        Ok(())
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
        let index = self
            .reference_map
            .get(&addr)
            .copied()
            .expect("Object not found in reference map during serialization");
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
            let index = self.reference_map.get(&addr).copied().expect(
                "Object not found in reference map during serialization of non-immediate value",
            );

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

    #[allow(dead_code)]
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

    pub fn write_native_proc(&mut self, proc: Gc<'gc, NativeProc>) -> std::io::Result<()> {
        let index = self
            .native_procedures
            .iter()
            .position(|&addr| addr == proc.proc)
            .unwrap_or_else(|| {
                panic!(
                    "Native procedure at {} not found in native procedures list during serialization",
                    proc.proc
                )
            });

        self.write32(index as u32)?;

        Ok(())
    }

    /// The *most* complex object to serialize: closures.
    ///
    /// Requires finding out dynamic library its code comes from, or native function
    /// it belongs to (if any), and then serializing this reference accordingly.
    #[allow(unused)]
    pub fn write_closure(&mut self, closure: ClosureRef<'gc>) -> std::io::Result<()> {
        self.write_value(closure.meta.get())?;
        self.write_value(closure.free)?;

        let code = closure.code;

        // code defined in native
        if code == get_trampoline_from_scheme() || code == get_cont_trampoline_from_scheme() {
            let native_proc = closure.free.downcast::<Vector>()[0]
                .get()
                .downcast::<NativeProc>();
        }

        Ok(())
    }

    pub fn write_pointer(&mut self, ptr: Gc<'gc, Pointer>) -> std::io::Result<()> {
        if !ptr.value.is_zero() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Cannot serialize Pointer with non-zero value",
            ));
        }

        Ok(())
    }

    pub fn write_cmarks(&mut self, cmarks: Gc<'gc, ContinuationMarks>) -> std::io::Result<()> {
        let mut cur = cmarks.cmarks;
        while let Some(marks) = cur {
            self.write_value(marks.mark_set.get())?;
            self.write_value(marks.retk.into())?;
            cur = marks.parent;
        }
        Ok(())
    }

    /// Write globals aka inherents into the image.
    pub fn write_globals(&mut self) {
        self.ctx.globals().for_each_value(|val| {
            self.write_value(val.get())
                .expect("Failed to write global value");
        });
    }
}

/// A helper struct for building the reference map during the first
/// pass of serialization.
pub struct ReferenceMapBuilder<'gc> {
    pub ctx: Context<'gc>,
    pub reference_map: BTreeMap<Address, u32>,
    pub to_process: Vec<Value<'gc>>,
}

impl<'gc> ReferenceMapBuilder<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self {
            ctx,
            reference_map: BTreeMap::new(),
            to_process: Vec::new(),
        }
    }

    pub fn scan_context(&mut self) {
        let symbols = symbol_table(&self.ctx);
        self.enqueue_value(symbols.into_value(self.ctx));

        let globals = self.ctx.globals();
        globals.for_each_value(|val| {
            self.enqueue_value(val.get());
        });

        let dynstate = self.ctx.dynamic_state();
        self.enqueue_value(dynstate);
        let marks = self.ctx.current_continuation_marks();
        self.enqueue_value(marks.into());
    }

    pub fn run_to_end(&mut self) {
        while let Some(val) = self.to_process.pop() {
            self.scan_value(val);
        }
    }

    pub fn scan_value(&mut self, val: Value<'gc>) {
        if val.is_immediate() {
            return;
        }

        if val.is_pair() {
            self.scan_pair(val);
        } else if let Some(dynstate) = val.try_as::<DynamicStateObject>() {
            self.scan_dynamic_state_object(dynstate);
        } else if let Some(fluid) = val.try_as::<Fluid>() {
            self.scan_fluid(fluid);
        } else if let Some(module) = val.try_as::<Module>() {
            self.scan_module(module);
        } else if let Some(var) = val.try_as::<Variable>() {
            self.scan_variable(var);
        } else if let Some(b) = val.try_as::<Boxed>() {
            self.scan_boxed(b);
        } else if let Some(ht) = val.try_as::<HashTable>() {
            self.scan_hashtable(ht);
        } else if let Some(num) = val.number() {
            self.scan_number(num);
        } else if let Some(closure) = val.try_as::<Closure>() {
            self.scan_closure(closure);
        } else if let Some(string) = val.try_as::<Str>() {
            self.scan_str(string);
        } else if let Some(symbol) = val.try_as::<Symbol>() {
            self.scan_symbol(symbol);
        } else if let Some(wset) = val.try_as::<WeakSet>() {
            self.scan_weakset(wset);
        } else if let Some(wmap) = val.try_as::<WeakMapping>() {
            self.scan_weakmapping(wmap);
        } else if let Some(wt) = val.try_as::<WeakTable>() {
            self.scan_weaktable(wt);
        } else if let Some(wcm) = val.try_as::<ContinuationMarks>() {
            self.scan_continuation_marks(wcm);
        } else if let Some(vector) = val.try_as::<Vector>() {
            self.scan_vector(vector);
        } else if let Some(tuple) = val.try_as::<Tuple>() {
            self.scan_tuple(tuple);
        } else if let Some(syntax) = val.try_as::<Syntax>() {
            self.scan_syntax(syntax);
        } else if let Some(transformer) = val.try_as::<SyntaxTransformer>() {
            self.scan_syntax_transformer(transformer);
        } else if let Some(_) = val.try_as::<ByteVector>() {
            // bytevectors have no references
        } else if let Some(_) = val.try_as::<NativeProc>() {
            // native procs have no references
        } else if let Some(_) = val.try_as::<Stringbuf>() {
            // stringbufs have no references
        } else if let Some(_) = val.try_as::<Pointer>() {
            // pointers have no references
        } else {
            panic!(
                "Unknown object type during reference map building: {val:?} {:?}",
                val.typ8().bits()
            );
        }
    }

    #[inline(never)]
    pub fn enqueue_value(&mut self, val: Value<'gc>) {
        if val.is_immediate() {
            return;
        }

        let addr = unsafe { Address::from_usize(val.bits() as _) };
        if self.reference_map.contains_key(&addr) {
            return;
        }

        let index = self.reference_map.len() as u32;
        self.reference_map.insert(addr, index);

        self.to_process.push(val);
    }

    pub fn scan_pair(&mut self, pair: Value<'gc>) {
        let car = pair.car();
        let cdr = pair.cdr();

        self.enqueue_value(car);
        self.enqueue_value(cdr);
    }

    pub fn scan_vector(&mut self, vector: Gc<'gc, Vector<'gc>>) {
        let len = vector.len();
        for i in 0..len {
            let elem = vector[i].get();
            self.enqueue_value(elem);
        }
    }

    pub fn scan_tuple(&mut self, tuple: Gc<'gc, Tuple<'gc>>) {
        let len = tuple.len();
        for i in 0..len {
            let elem = tuple[i].get();
            self.enqueue_value(elem);
        }
    }

    pub fn scan_dynamic_state_object(&mut self, obj: Gc<'gc, DynamicStateObject>) {
        let state = obj.saved;
        self.enqueue_value(state);
    }

    pub fn scan_fluid(&mut self, fluid: Gc<'gc, Fluid<'gc>>) {
        let value = fluid.value;
        self.enqueue_value(value);
    }

    pub fn scan_module(&mut self, module: Gc<'gc, Module<'gc>>) {
        let obarray = module.obarray.get();
        let uses = module.uses.get();
        let binder = module.binder;
        let _declarative = module.declarative.get();
        let transformer = module.transformer;
        let name = module.name.get();
        let version = module.version.get();
        let _kind = module.kind.get();
        let import_obarray = module.import_obarray;
        let submodules = module.submodules;
        let filename = module.filename.get();
        let public_interface = module.public_interface.get();
        let replacements = module.replacements.get();
        let inlinable_exports = module.inlinable_exports.get();
        let environment = module.environment.get();

        self.enqueue_value(obarray.into());
        self.enqueue_value(uses);
        self.enqueue_value(binder);
        self.enqueue_value(transformer);
        self.enqueue_value(name);
        self.enqueue_value(version);
        self.enqueue_value(import_obarray.into());
        self.enqueue_value(submodules.into());
        self.enqueue_value(filename);
        if let Some(iface) = public_interface {
            self.enqueue_value(iface.into());
        }
        self.enqueue_value(replacements);
        self.enqueue_value(inlinable_exports);
        self.enqueue_value(environment);
    }

    pub fn scan_variable(&mut self, var: Gc<'gc, Variable<'gc>>) {
        self.enqueue_value(var.value.get());
    }

    pub fn scan_boxed(&mut self, b: Gc<'gc, Boxed<'gc>>) {
        let value = b.val;
        self.enqueue_value(value);
    }

    pub fn scan_hashtable(&mut self, ht: Gc<'gc, HashTable<'gc>>) {
        for (key, val) in ht.iter() {
            self.enqueue_value(key);
            self.enqueue_value(val);
        }
    }

    pub fn scan_number(&mut self, n: Number<'gc>) {
        match n {
            Number::Fixnum(_) | Number::Flonum(_) => {
                // nothing to do
            }

            Number::BigInt(_bn) => {}

            Number::Complex(cn) => {
                self.enqueue_value(cn.real.into_value(self.ctx));
                self.enqueue_value(cn.imag.into_value(self.ctx));
            }

            Number::Rational(rn) => {
                self.enqueue_value(rn.numerator.into_value(self.ctx));
                self.enqueue_value(rn.denominator.into_value(self.ctx));
            }
        }
    }

    pub fn scan_closure(&mut self, closure: ClosureRef<'gc>) {
        self.enqueue_value(closure.meta.get());
        self.enqueue_value(closure.free);
    }

    pub fn scan_str(&mut self, string: Gc<'gc, Str<'gc>>) {
        let buf = string.stringbuf.get();
        self.enqueue_value(buf.into());
    }

    pub fn scan_symbol(&mut self, symbol: Gc<'gc, Symbol<'gc>>) {
        let strbuf = symbol.stringbuf;
        self.enqueue_value(strbuf.into());
    }

    pub fn scan_weakset(&mut self, wset: Gc<'gc, WeakSet<'gc>>) {
        let mut elems = Vec::with_capacity(4);
        wset.for_each(&self.ctx, |elem| {
            elems.push(elem);
        });

        for elem in elems {
            self.enqueue_value(elem);
        }
    }

    pub fn scan_weakmapping(&mut self, wmap: Gc<'gc, WeakMapping<'gc>>) {
        let key = wmap.key(&self.ctx);
        let value = wmap.value(&self.ctx);

        self.enqueue_value(key);
        self.enqueue_value(value);
    }

    pub fn scan_weaktable(&mut self, ht: Gc<'gc, WeakTable<'gc>>) {
        ht.for_each(self.ctx, |key, val| {
            self.enqueue_value(key);
            self.enqueue_value(val);
        });
    }

    pub fn scan_continuation_marks(&mut self, wcm: Gc<'gc, ContinuationMarks<'gc>>) {
        let mut cmarks = wcm.cmarks;

        while let Some(marks) = cmarks {
            self.enqueue_value(marks.mark_set.get());
            self.enqueue_value(marks.retk.into());
            cmarks = marks.parent;
        }
    }

    pub fn scan_syntax(&mut self, syntax: Gc<'gc, Syntax<'gc>>) {
        self.enqueue_value(syntax.expr);
        self.enqueue_value(syntax.wrap);
        self.enqueue_value(syntax.module);
        self.enqueue_value(syntax.source);
    }

    pub fn scan_syntax_transformer(&mut self, transformer: Gc<'gc, SyntaxTransformer<'gc>>) {
        let name = transformer.name();
        let typ = transformer.typ();
        let binding = transformer.binding();

        self.enqueue_value(name);
        self.enqueue_value(typ);
        self.enqueue_value(binding);
    }
}
