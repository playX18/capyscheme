use crate::rsgc::{
    Gc,
    mmtk::util::Address,
    object::{GCObject, HeapObjectHeader},
};
use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    ffi::CStr,
    io::Write,
    mem::MaybeUninit,
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
        image::ALLOWED_GC,
        modules::Variable,
        vm::{
            control::ContinuationMarks,
            dl::OPEN_HANDLES,
            ffi::{CIF, Pointer},
            libraries::LIBRARY_COLLECTION,
            syntax::{Syntax, SyntaxTransformer},
            thunks::current_module,
            trampolines::{get_cont_trampoline_from_scheme, get_trampoline_from_scheme},
        },
    },
};

type Result<'gc, T = ()> = std::result::Result<T, ImageBuildError<'gc>>;

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
pub const TYPE_CLOSURE_SCM: u8 = 20;
/// Native closure: read from vec of native function pointers
pub const TYPE_CLOSURE_NATIVE_PROC: u8 = 21;
pub const TYPE_CLOSURE_NATIVE_CONT: u8 = 22;

pub const TYPE_POINTER_NULL: u8 = 30;
pub const TYPE_POINTER_DYLIB: u8 = 31;

pub struct DynLibInfo {
    pub handle: Address,
    pub fname: Option<String>,
    pub flags: u32,
    pub snames: HashMap<String, usize>,
}

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
    pub fbase_names: BTreeMap<usize, Vec<String>>,
    pub native_procedures: Vec<Address>,

    pub dylibs: Vec<DynLibInfo>,
}

impl<'gc> ImageBuilder<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self {
            ctx,
            output: Vec::with_capacity(32 * 1024 * 1024),
            reference_map: BTreeMap::new(),
            to_serialize: Vec::new(),
            fbase_to_lib: BTreeMap::new(),
            native_procedures: Vec::new(),
            fbase_names: BTreeMap::new(),
            dylibs: Vec::new(),
        }
    }

    pub fn serialize(&mut self, proc: Value<'gc>) -> Result<'gc, Vec<u8>> {
        let mut scan = ReferenceMapBuilder::new(self.ctx);

        scan.scan_context(proc);
        scan.run_to_end()?;

        self.dylibs = scan.dylibs;
        self.reference_map = scan.reference_map;
        self.native_procedures = super::all_native_procedures(self.ctx);
        self.fbase_to_lib = scan.fbase_to_lib;
        self.fbase_names = scan.fbase_names;

        self.write8((*ALLOWED_GC.get().unwrap()) as u8)?;
        self.serialize_libraries()?;
        self.serialize_dylibs()?;

        self.serialize_reference_map()?;

        {
            self.write_symbol_table()?;
            self.write_globals();

            let dynstate = self.ctx.dynamic_state();
            let winders = self.ctx.winders();
            let marks = self.ctx.state().current_marks();
            self.write_value(dynstate)?;
            self.write_value(winders)?;
            self.write_value(marks)?;

            let current_module = current_module(self.ctx);
            self.write_value(current_module)?;
            self.write_value(proc)?;
        }

        Ok(std::mem::take(&mut self.output))
    }

    fn serialize_reference_map(&mut self) -> Result<'gc, ()> {
        self.write32(self.reference_map.len() as u32)?;

        let objects = self.reference_map.clone();
        let mut objects_vec = objects
            .iter()
            .map(|x| (x.0.clone(), x.1.clone()))
            .collect::<Vec<_>>();
        objects_vec.sort_by(|a, b| a.1.cmp(&b.1));
        for (addr, index) in objects_vec.into_iter() {
            let gc_object = GCObject::from_address(addr);

            let obj_start = self.write32(0)?;
            self.write32(index)?;
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
            } else if let Some(cif) = val.try_as::<CIF>() {
                self.write_cif(cif)?;
            } else {
                panic!(
                    "Unknown object type during serialization: {val:?} {:?}",
                    val.typ8().bits()
                );
            }

            let ser_end = self.output.len();
            self.patch32(obj_start, ser_end as _)?;
        }

        Ok(())
    }

    fn write_symbol_table(&mut self) -> Result<'gc> {
        let sym_table = symbol_table(*self.ctx);
        self.write_reference(sym_table)?;
        Ok(())
    }

    fn serialize_dylibs(&mut self) -> Result<'gc> {
        self.write16(self.dylibs.len() as u16)?;
        for i in 0..self.dylibs.len() {
            match self.dylibs[i].fname.clone() {
                Some(fname) => {
                    self.write32(fname.len() as u32)?;
                    self.write_many(fname.as_bytes())?;
                }

                None => {
                    self.write32(0)?;
                }
            }
            self.write32(self.dylibs[i].flags as u32)?;
            let mut snames = self.dylibs[i]
                .snames
                .iter()
                .map(|(sname, ix)| (sname.clone(), *ix))
                .collect::<Vec<_>>();

            snames.sort_by(|a, b| a.1.cmp(&b.1));
            self.write32(snames.len() as u32)?;
            for (sname, ix) in snames.into_iter() {
                self.write32(ix as u32)?;
                self.write32(sname.len() as u32)?;
                self.write_many(sname.as_bytes())?;
            }
        }

        Ok(())
    }

    /// Walk all the loaded dynamic libraries and serialize them into the image. It performs ZSTD
    /// compression on each library ELF binary.
    fn serialize_libraries(&mut self) -> Result<'gc, ()> {
        let libs = LIBRARY_COLLECTION.fetch(*self.ctx);

        let mut libraries = Vec::new();

        libs.for_each_library(|lib| {
            let file = std::fs::File::open(&lib.path).expect("Failed to open library file");

            let mut reader = std::io::BufReader::new(file);
            let mut buffer = Vec::with_capacity(1024 * 1024);

            std::io::copy(&mut reader, &mut buffer).expect("Failed to read library file");
            let mut lib_globals = Vec::with_capacity(128);
            lib.for_each_global(|global| {
                lib_globals.push(*global);
            });

            libraries.push((lib.fbase, buffer, lib_globals));
        });
        let cstart = self.write16(0)?;

        let mut index = 0;
        for (_i, (_, bin, globals)) in libraries.into_iter().enumerate() {
            let size = bin.len() as u64;

            self.write64(size)?;
            self.write_many(&bin)?;
            let names = self.fbase_names.get(&index).cloned().unwrap_or_default();
            self.write32(names.len() as u32)?;
            for sname in names {
                self.write32(sname.len() as u32)?;
                self.write_many(sname.as_bytes())?;
            }

            let globals_end_pos = self.write32(0)?;
            self.write32(globals.len() as u32)?;
            for global in globals {
                self.write_value(global)?;
            }

            let globals_end = self.output.len();
            self.patch32(globals_end_pos, globals_end as _)?;

            index += 1;
        }

        self.patch16(cstart, index as u16)?;

        Ok(())
    }

    pub fn write_many(&mut self, bytes: &[u8]) -> Result<'gc, usize> {
        let pos = self.output.len();
        self.output.write_all(bytes)?;
        Ok(pos)
    }

    pub fn write8(&mut self, byte: u8) -> Result<'gc, usize> {
        let pos = self.output.len();
        self.output.write_all(&[byte])?;
        Ok(pos)
    }

    pub fn write16(&mut self, value: u16) -> Result<'gc, usize> {
        let pos = self.output.len();
        self.output.write_all(&value.to_le_bytes())?;
        Ok(pos)
    }

    pub fn write32(&mut self, value: u32) -> Result<'gc, usize> {
        let pos = self.output.len();
        self.output.write_all(&value.to_le_bytes())?;
        Ok(pos)
    }

    pub fn write64(&mut self, value: u64) -> Result<'gc, usize> {
        let pos = self.output.len();
        self.output.write_all(&value.to_le_bytes())?;
        Ok(pos)
    }

    pub fn patch8(&mut self, position: usize, byte: u8) -> Result<'gc, ()> {
        self.output[position] = byte;
        Ok(())
    }

    pub fn patch16(&mut self, position: usize, value: u16) -> Result<'gc, ()> {
        let bytes = value.to_le_bytes();
        self.output[position..position + 2].copy_from_slice(&bytes);
        Ok(())
    }

    pub fn patch32(&mut self, position: usize, value: u32) -> Result<'gc, ()> {
        let bytes = value.to_le_bytes();
        self.output[position..position + 4].copy_from_slice(&bytes);
        Ok(())
    }

    pub fn patch64(&mut self, position: usize, value: u64) -> Result<'gc, ()> {
        let bytes = value.to_le_bytes();
        self.output[position..position + 8].copy_from_slice(&bytes);
        Ok(())
    }

    pub fn write_reference<T>(&mut self, object: Gc<'gc, T>) -> Result<'gc, usize>
    where
        T: Tagged,
    {
        self.write8(TYPE_REF)?;
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
    pub fn write_value(&mut self, val: Value<'gc>) -> Result<'gc, usize> {
        if val.is_immediate() {
            /*if val.is_bool() {
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
            } else {*/
            self.write8(TYPE_IMMEDIATE)?;
            self.write64(val.bits() as u64)
            //}
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

    pub fn write_pair(&mut self, pair: Value<'gc>) -> Result<'gc, ()> {
        let car = pair.car();
        let cdr = pair.cdr();
        self.write_value(car)?;
        self.write_value(cdr)?;
        Ok(())
    }

    pub fn write_vector(&mut self, vector: Gc<'gc, Vector<'gc>>) -> Result<'gc, ()> {
        let len = vector.len();
        self.write32(len as u32)?;
        for i in 0..len {
            let elem = vector[i].get();
            self.write_value(elem)?;
        }
        Ok(())
    }

    pub fn write_tuple(&mut self, tuple: Gc<'gc, Tuple<'gc>>) -> Result<'gc, ()> {
        // len is stored in header
        for i in 0..tuple.len() {
            let elem = tuple[i].get();
            self.write_value(elem)?;
        }
        Ok(())
    }

    pub fn write_bytevector(&mut self, bytevector: Gc<'gc, ByteVector>) -> Result<'gc, ()> {
        if bytevector.is_mapping() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Cannot serialize mapped ByteVector",
            )
            .into());
        }

        let len = bytevector.len();
        self.write64(len as u64)?;
        let bytes = bytevector.as_slice();
        self.output.write_all(bytes).map_err(|e| e.into())
    }

    #[allow(dead_code)]
    pub(crate) fn write_stringbuf(&mut self, string: Gc<'gc, Stringbuf>) -> Result<'gc, ()> {
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

    pub fn write_string(&mut self, string: Gc<'gc, Str<'gc>>) -> Result<'gc, ()> {
        let length = string.len();
        let start = string.start.get();
        let buf = string.stringbuf.get();

        self.write32(start as u32)?;
        self.write32(length as u32)?;
        self.write_reference(buf)?;
        Ok(())
    }

    pub fn write_symbol(&mut self, symbol: Gc<'gc, Symbol<'gc>>) -> Result<'gc, ()> {
        let strbuf = symbol.stringbuf;
        self.write_reference(strbuf)?;
        Ok(())
    }

    pub fn write_number(&mut self, n: Number<'gc>) -> Result<'gc, ()> {
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

    pub fn write_box(&mut self, b: Gc<'gc, Boxed<'gc>>) -> Result<'gc, ()> {
        let value = b.val;
        self.write_value(value)?;
        Ok(())
    }

    pub fn write_module(&mut self, module: Gc<'gc, Module<'gc>>) -> Result<'gc, ()> {
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

    pub fn write_variable(&mut self, var: Gc<'gc, Variable<'gc>>) -> Result<'gc, ()> {
        self.write_value(var.value.get())?;
        Ok(())
    }

    pub fn write_hashtable(&mut self, ht: Gc<'gc, HashTable<'gc>>) -> Result<'gc, ()> {
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

    pub fn write_weaktable(&mut self, ht: Gc<'gc, WeakTable<'gc>>) -> Result<'gc, ()> {
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

    pub fn write_weakmapping(&mut self, wmap: Gc<'gc, WeakMapping<'gc>>) -> Result<'gc, ()> {
        let key = wmap.key(*self.ctx);
        let value = wmap.value(*self.ctx);

        self.write_value(key)?;
        self.write_value(value)?;

        Ok(())
    }

    pub fn write_weakset(&mut self, wset: Gc<'gc, WeakSet<'gc>>) -> Result<'gc, ()> {
        let inner = wset.inner.lock();
        let entries = inner.entries.get();

        // write count of collected elements since `wset.len()` may be inaccurate and `for_each` may
        // vacuum dead entries.
        let count = entries.len();
        self.write32(count as u32)?;
        for entry in entries.iter() {
            self.write_value(entry.get().value)?;
            self.write64(entry.get().hash)?;
        }

        self.write32(inner.size.get() as u32)?;
        self.write32(inner.n_items.get() as u32)?;
        self.write32(inner.lower.get() as u32)?;
        self.write32(inner.upper.get() as u32)?;
        self.write32(inner.size_index.get() as u32)?;
        self.write32(inner.min_size_index.get() as u32)?;

        Ok(())
    }

    pub fn write_syntax(&mut self, syntax: Gc<'gc, Syntax<'gc>>) -> Result<'gc, ()> {
        self.write_value(syntax.expr)?;
        self.write_value(syntax.wrap)?;
        self.write_value(syntax.module)?;
        self.write_value(syntax.source)?;

        Ok(())
    }

    pub fn write_syntax_transformer(
        &mut self,
        transformer: Gc<'gc, SyntaxTransformer<'gc>>,
    ) -> Result<'gc, ()> {
        let name = transformer.name();
        let typ = transformer.typ();
        let binding = transformer.binding();

        self.write_value(name)?;
        self.write_value(typ)?;
        self.write_value(binding)?;

        Ok(())
    }

    pub fn write_fluid(&mut self, fluid: Gc<'gc, Fluid<'gc>>) -> Result<'gc, ()> {
        let value = fluid.value;
        self.write_value(value)?;
        Ok(())
    }

    pub fn write_dynamic_state_object(
        &mut self,
        obj: Gc<'gc, DynamicStateObject>,
    ) -> Result<'gc, ()> {
        let state = obj.saved;
        self.write_value(state)?;
        Ok(())
    }

    pub fn write_native_proc(&mut self, proc: Gc<'gc, NativeProc>) -> Result<'gc, ()> {
        let index = self
            .native_procedures
            .iter()
            .position(|&addr| addr == proc.proc)
            .unwrap_or_else(|| {
                let dlinfo = crate::utils::dladdr(proc.proc).expect(
                    "Failed to get dladdr info for native procedure during serialization",
                );
                println!(
                    "Native procedure at {} not found in native procedures list during serialization. Dl info: fname={:?}, sname={:?}",
                    proc.proc, dlinfo.fname, dlinfo.sname
                );
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
    pub fn write_closure(&mut self, closure: ClosureRef<'gc>) -> Result<'gc, ()> {
        self.write_value(closure.meta.get())?;
        self.write_value(closure.free)?;

        let code = closure.code;

        // code defined in native
        if code == get_trampoline_from_scheme() {
            self.write8(TYPE_CLOSURE_NATIVE_PROC)?;
            return Ok(());
        } else if code == get_cont_trampoline_from_scheme() {
            self.write8(TYPE_CLOSURE_NATIVE_CONT)?;
            return Ok(());
        }

        // write dynamic library closure

        let (lib_index, symbol_index) = self
            .find_library_for_code(code)
            .expect("Failed to find library for closure code during serialization");
        self.write8(TYPE_CLOSURE_SCM)?;
        self.write16(lib_index)?;
        self.write32(symbol_index)?;

        Ok(())
    }

    pub fn write_pointer(&mut self, ptr: Gc<'gc, Pointer>) -> Result<'gc, ()> {
        if ptr.value.is_zero() {
            self.write8(TYPE_POINTER_NULL)?;
        } else {
            let dlinfo = crate::utils::dladdr(ptr.value)?;
            let lib_index = self
                .dylibs
                .iter()
                .position(|lib| lib.fname == dlinfo.fname)
                .ok_or_else(|| {
                    std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!(
                            "Failed to find dynamic library {:?} for pointer during serialization",
                            dlinfo.fname
                        ),
                    )
                })?;

            // write (library index, offset from symbol address, symbol index)
            self.write8(TYPE_POINTER_DYLIB)?;
            self.write16(lib_index as u16)?;
            let offset =
                (ptr.value.as_usize() as isize).wrapping_sub(dlinfo.saddr.as_usize() as isize);
            self.write32(offset as i32 as u32)?;

            let sname = dlinfo.sname.ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "Failed to get symbol name for pointer during serialization (missed during scan?)",
                )
            })?;

            let sym_index = match self.dylibs[lib_index].snames.get(&sname) {
                Some(ix) => *ix,
                None => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!(
                            "Failed to find symbol name {:?} in library {:?} during serialization",
                            sname, self.dylibs[lib_index].fname
                        ),
                    )
                    .into());
                }
            };

            self.write32(sym_index as u32)?;
        }

        Ok(())
    }

    pub fn write_cif(&mut self, cif: Gc<'gc, CIF>) -> Result<'gc, ()> {
        self.write32(cif.nargs)?;
        self.write8(cif.variadic as u8)?;
        self.write_value(cif.args)?;
        self.write_value(cif.return_type)?;

        Ok(())
    }

    pub fn write_cmarks(&mut self, cmarks: Gc<'gc, ContinuationMarks>) -> Result<'gc, ()> {
        self.write_value(cmarks.cmarks)?;
        Ok(())
    }

    /// Write globals aka inherents into the image.
    pub fn write_globals(&mut self) {
        self.ctx.globals().for_each_value(|val| {
            self.write_value(val.get())
                .expect("Failed to write global value");
        });
    }

    fn find_library_for_code(&mut self, code: Address) -> Option<(u16, u32)> {
        unsafe {
            let mut dl_info: MaybeUninit<libc::Dl_info> = MaybeUninit::uninit();
            let result = libc::dladdr(code.to_mut_ptr(), dl_info.as_mut_ptr());
            if result == 0 {
                return None;
            }
            let dl_info = dl_info.assume_init();
            let fbase = Address::from_ptr(dl_info.dli_fbase as *mut _);
            let lib_index = self.fbase_to_lib[&fbase];
            let sname = CStr::from_ptr(dl_info.dli_sname);
            let symbols = self.fbase_names.entry(lib_index).or_insert(Vec::new());
            let symbol_index = match symbols
                .iter()
                .position(|s| s.as_str() == sname.to_str().unwrap())
            {
                Some(ix) => ix as u32,
                None => return None,
            };

            Some((lib_index as u16, symbol_index))
        }
    }
}

/// A helper struct for building the reference map during the first
/// pass of serialization.
pub struct ReferenceMapBuilder<'gc> {
    pub ctx: Context<'gc>,
    pub reference_map: BTreeMap<Address, u32>,
    pub dylibs: Vec<DynLibInfo>,
    pub to_process: Vec<Value<'gc>>,
    pub fbase_to_lib: BTreeMap<Address, usize>,
    pub fbase_names: BTreeMap<usize, Vec<String>>,
    pub recursion_level: usize,
}

impl<'gc> ReferenceMapBuilder<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        Self {
            ctx,
            reference_map: BTreeMap::new(),
            to_process: Vec::with_capacity(1024),
            dylibs: Vec::new(),
            fbase_names: BTreeMap::new(),
            fbase_to_lib: BTreeMap::new(),
            recursion_level: 0,
        }
    }

    pub fn scan_context(&mut self, proc: Value<'gc>) {
        OPEN_HANDLES.lock().iter().for_each(|(handle, dl)| {
            self.dylibs.push(DynLibInfo {
                handle: *handle,
                fname: dl.path.clone().map(|x| x.to_string_lossy().to_string()),
                flags: dl.flags as _,
                snames: HashMap::new(),
            })
        });
        let mut index = 0;

        let mut globals_to_scan = Vec::with_capacity(128);

        LIBRARY_COLLECTION.fetch(*self.ctx).for_each_library(|lib| {
            let fbase = lib.fbase;
            self.fbase_to_lib.insert(fbase, index);

            lib.for_each_global(|val| {
                globals_to_scan.push(*val);
            });

            index += 1;
        });
        for global in globals_to_scan {
            self.enqueue_value(global);
        }

        let symbols = symbol_table(*self.ctx);
        self.enqueue_value(symbols.into_value(self.ctx));

        let globals = self.ctx.globals();
        globals.for_each_value(|val| {
            self.enqueue_value(val.get());
        });

        let dynstate = self.ctx.dynamic_state();
        let winders = self.ctx.winders();
        self.enqueue_value(dynstate);
        self.enqueue_value(winders);
        let cur = self.ctx.state().current_marks();
        self.enqueue_value(cur);
        let current_module = current_module(self.ctx);
        self.enqueue_value(current_module);
        self.enqueue_value(proc);
    }

    pub fn run_to_end(&mut self) -> Result<'gc> {
        while let Some(val) = self.to_process.pop() {
            self.scan_value(val)?;
        }

        Ok(())
    }

    pub fn scan_value(&mut self, val: Value<'gc>) -> Result<'gc> {
        if val.is_immediate() {
            return Ok(());
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
            self.scan_closure(closure)?;
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
        } else if let Some(cif) = val.try_as::<CIF>() {
            self.scan_cif(cif);
        } else if let Some(_) = val.try_as::<ByteVector>() {
            // bytevectors have no references
        } else if let Some(_) = val.try_as::<NativeProc>() {
            // native procs have no references
        } else if let Some(_) = val.try_as::<Stringbuf>() {
            // stringbufs have no references
        } else if let Some(ptr) = val.try_as::<Pointer>() {
            self.scan_pointer(ptr)?;
        } else {
            return Err(ImageBuildError::UnsupportedValue(val));
        }

        Ok(())
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

    pub fn scan_pointer(&mut self, ptr: Gc<'gc, Pointer>) -> Result<'gc> {
        if ptr.value.is_zero() {
            return Ok(());
        }
        let dlinfo = crate::utils::dladdr(ptr.value)?;
        let lib_index = self
            .dylibs
            .iter()
            .position(|lib| lib.fname == dlinfo.fname)
            .ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!(
                        "Failed to find dynamic library {:?} for pointer during serialization",
                        dlinfo.fname
                    ),
                )
            })?;
        let sname = dlinfo.sname.ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::Other,
                "Failed to get symbol name for pointer during serialization",
            )
        })?;

        let _ = match self.dylibs[lib_index].snames.get(&sname) {
            None => {
                let new_index = self.dylibs[lib_index].snames.len();
                self.dylibs[lib_index]
                    .snames
                    .insert(sname.clone(), new_index);
            }
            _ => (),
        };

        Ok(())
    }

    pub fn scan_closure(&mut self, closure: ClosureRef<'gc>) -> Result<'gc> {
        self.enqueue_value(closure.meta.get());
        self.enqueue_value(closure.free);

        if closure.code == get_trampoline_from_scheme()
            || closure.code == get_cont_trampoline_from_scheme()
        {
            return Ok(());
        }

        let _ = self.find_library_for_code(closure.code);

        Ok(())
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
        wset.for_each(*self.ctx, |elem| {
            elems.push(elem);
        });

        for elem in elems {
            self.enqueue_value(elem);
        }
    }

    pub fn scan_weakmapping(&mut self, wmap: Gc<'gc, WeakMapping<'gc>>) {
        let key = wmap.key(*self.ctx);
        let value = wmap.value(*self.ctx);

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
        self.enqueue_value(wcm.cmarks);
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

    pub fn scan_cif(&mut self, cif: Gc<'gc, CIF>) {
        self.enqueue_value(cif.args);
        self.enqueue_value(cif.return_type);
    }

    fn find_library_for_code(&mut self, code: Address) -> Option<(u16, u32)> {
        unsafe {
            let mut dl_info: MaybeUninit<libc::Dl_info> = MaybeUninit::uninit();
            let result = libc::dladdr(code.to_mut_ptr(), dl_info.as_mut_ptr());
            if result == 0 {
                return None;
            }
            let dl_info = dl_info.assume_init();
            let fbase = Address::from_ptr(dl_info.dli_fbase as *mut _);
            let lib_index = self.fbase_to_lib[&fbase];
            let sname = CStr::from_ptr(dl_info.dli_sname);
            let symbols = self.fbase_names.entry(lib_index).or_insert(Vec::new());
            let symbol_index = match symbols
                .iter()
                .position(|s| s.as_str() == sname.to_str().unwrap())
            {
                Some(ix) => ix as u32,
                None => {
                    symbols.push(sname.to_str().unwrap().to_string());
                    (symbols.len() - 1) as u32
                }
            };

            Some((lib_index as u16, symbol_index))
        }
    }
}

#[derive(Debug)]
pub enum ImageBuildError<'gc> {
    Io(std::io::Error),
    UnsupportedValue(Value<'gc>),
    UnsupportedFeature(Cow<'static, str>),
}

impl std::fmt::Display for ImageBuildError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImageBuildError::Io(err) => write!(f, "IO error during image building: {}", err),
            ImageBuildError::UnsupportedValue(val) => {
                write!(f, "Unsupported value during image building: {val:?}")
            }
            ImageBuildError::UnsupportedFeature(feature) => {
                write!(f, "Unsupported feature during image building: {feature}")
            }
        }
    }
}

impl<'gc> From<std::io::Error> for ImageBuildError<'gc> {
    fn from(err: std::io::Error) -> Self {
        ImageBuildError::Io(err)
    }
}
