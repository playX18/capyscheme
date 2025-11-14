use std::{
    cell::Cell,
    ffi::CString,
    io::{Cursor, Read},
    sync::{LazyLock, atomic::AtomicUsize},
};

use crate::rsgc::{
    alloc::Array,
    cell::Lock,
    mmtk::{AllocationSemantics, util::Address},
    object::{GCObject, VTableOf},
    sync::monitor::Monitor,
};
use easy_bitfield::BitFieldTrait;
use parking_lot::Mutex;
use tempfile::NamedTempFile;

use crate::{
    prelude::*,
    runtime::{
        Context,
        fluids::{DynamicStateObject, Fluid},
        global::{Globals, VM_GLOBALS},
        image::builder::{
            TYPE_BWP, TYPE_CHAR, TYPE_CLOSURE_NATIVE_CONT, TYPE_CLOSURE_NATIVE_PROC,
            TYPE_CLOSURE_SCM, TYPE_EOF, TYPE_FALSE, TYPE_I32, TYPE_IMMEDIATE, TYPE_NULL,
            TYPE_POINTER_DYLIB, TYPE_POINTER_NULL, TYPE_REF, TYPE_TRUE, TYPE_UNDEFINED,
        },
        modules::{ModuleKind, Variable},
        vm::{
            VMResult,
            control::ContinuationMarks,
            ffi::{CIF, Pointer},
            libraries::LIBRARY_COLLECTION,
            syntax::{Syntax, SyntaxTransformer},
            trampolines::{get_cont_trampoline_from_scheme, get_trampoline_from_scheme},
        },
    },
};

/// A list of temporary files to keep uncompressed libraries alive.
///
/// These files should be cleaned up when program exits.
static TEMPORARY_FILES: LazyLock<Mutex<Vec<NamedTempFile>>> =
    LazyLock::new(|| Mutex::new(Vec::new()));

pub struct ImageReader<'gc, R: AsRef<[u8]>> {
    ctx: Context<'gc>,
    input: Cursor<R>,

    reference_map: Vec<Value<'gc>>,
    libraries_end: usize,
    reference_map_start: usize,
    reference_map_end: usize,
    scm_libs: Vec<(Address, Value<'gc>, Vec<Address>, usize)>,
    dylibs: Vec<(Address, Vec<Address>)>,
    native_procedures: Vec<Address>,
}

#[derive(Trace)]
pub struct Image<'gc> {
    /// User entrypoint in the image.
    entrypoint: Value<'gc>,
    /// Entrypoints for all loaded Scheme libraries.
    /// Must be called first.
    library_entrypoints: Vec<Value<'gc>>,
}

impl<'gc> Image<'gc> {
    pub fn boot(self, _ctx: Context<'gc>) -> VMResult<'gc> {
        VMResult::Ok(self.entrypoint)
    }
}

impl<'gc, R: AsRef<[u8]>> ImageReader<'gc, R> {
    pub fn new(ctx: Context<'gc>, input: R) -> Self {
        Self {
            ctx,
            input: Cursor::new(input),
            scm_libs: Vec::new(),
            reference_map: Vec::new(),
            libraries_end: 0,
            reference_map_start: 0,
            reference_map_end: 0,
            dylibs: Vec::new(),
            native_procedures: Vec::new(),
        }
    }

    //#[track_caller]
    pub fn deserialize(&mut self) -> std::io::Result<Image<'gc>> {
        self.native_procedures = super::all_native_procedures(self.ctx);
        let _ = self.read8()?;
        self.uncompress_libraries()?;
        self.read_dylibs()?;
        self.allocate_references()?;
        self.initialize_references()?;
        self.read_libraries_globals()?;

        self.input.set_position(self.reference_map_end as u64);

        let symtab = unsafe { self.read_value()?.downcast_unchecked::<WeakSet>() };
        crate::runtime::value::symbols::SYMBOL_TABLE
            .set(crate::rsgc::Global::new(symtab))
            .map_err(|_| {
                std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "Failed to set global symbol table",
                )
            })
            .unwrap();
        self.read_globals().unwrap();

        let dynstate = self.read_value().unwrap();
        self.ctx.set_dynamic_state(dynstate);
        let winders = self.read_value().unwrap();
        self.ctx.set_winders(winders);
        let marks = self.read_value()?;
        unsafe {
            self.ctx.state().set_current_marks(marks);
        }

        let current_module = self.read_value().unwrap();

        crate::runtime::modules::current_module(self.ctx).set(self.ctx, current_module);
        let entrypoint = self.read_value().unwrap();
        let library_entrypoints = self
            .scm_libs
            .drain(..)
            .map(|(_fbase, entrypoint, _syms, _)| entrypoint)
            .collect();

        Ok(Image {
            entrypoint,
            library_entrypoints,
        })
    }

    fn read_libraries_globals(&mut self) -> std::io::Result<()> {
        let old_pos = self.input.position() as usize;
        let cstarts = self
            .scm_libs
            .iter()
            .map(|(_, _, _, cstart)| *cstart)
            .collect::<Vec<_>>();

        let mut ix = 0;
        LIBRARY_COLLECTION.fetch(*self.ctx).for_each_library(|lib| {
            let cstart = *cstarts.get(ix).unwrap();
            self.input.set_position(cstart as u64);
            let _globals_count = self.read32().unwrap() as usize;
            lib.for_each_global(|glob| {
                let val = self.read_value().unwrap();
                *glob = unsafe { std::mem::transmute(val) };
            });

            ix += 1;
        });

        self.input.set_position(old_pos as u64);

        Ok(())
    }

    pub fn read8(&mut self) -> std::io::Result<u8> {
        let mut buf = [0u8; 1];
        self.input.read_exact(&mut buf).unwrap();

        Ok(buf[0])
    }

    pub fn read16(&mut self) -> std::io::Result<u16> {
        let mut buf = [0u8; 2];
        self.input.read_exact(&mut buf).unwrap();

        Ok(u16::from_le_bytes(buf))
    }

    pub fn read32(&mut self) -> std::io::Result<u32> {
        let mut buf = [0u8; 4];
        self.input.read_exact(&mut buf).unwrap();

        Ok(u32::from_le_bytes(buf))
    }

    pub fn read64(&mut self) -> std::io::Result<u64> {
        let mut buf = [0u8; 8];
        self.input.read_exact(&mut buf).unwrap();

        Ok(u64::from_le_bytes(buf))
    }

    /// Uncompress all compressed libraries in the image.
    ////#[track_caller]
    fn uncompress_libraries(&mut self) -> std::io::Result<()> {
        let lib_count = self.read16()? as usize;
        let mut libs_syms = Vec::with_capacity(lib_count);
        for _i in 0..lib_count {
            let size = self.read64().unwrap();

            let mut file = NamedTempFile::new().unwrap();

            let mut decoder = self.input.by_ref().take(size); //zstd::Decoder::new(self.input.by_ref().take(size)).unwrap();

            std::io::copy(&mut decoder, &mut file).unwrap();

            TEMPORARY_FILES.lock().push(file);

            let snames_count = self.read32()? as usize;
            let mut snames = Vec::with_capacity(snames_count);
            for _ in 0..snames_count {
                let slen = self.read32()? as usize;
                let mut buf = vec![0u8; slen];
                self.input.read_exact(&mut buf).unwrap();
                let sname = String::from_utf8(buf)
                    .map_err(|e| {
                        std::io::Error::new(std::io::ErrorKind::Other, format!("UTF8 error: {}", e))
                    })
                    .unwrap();

                snames.push(sname);
            }

            let csection_end = self.read32()? as usize;
            let csection = self.input.position() as usize;
            self.input.set_position(csection_end as u64);
            libs_syms.push((snames, csection));
        }
        let libs = LIBRARY_COLLECTION.fetch(*self.ctx);
        for (i, file) in TEMPORARY_FILES.lock().iter().enumerate() {
            let path_str = file.path().to_string_lossy().to_string();
            let (fbase, handle, entrypoint) = libs
                .load_and_get_fbase(&path_str, self.ctx)
                .map_err(|e| {
                    std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!("Failed to load library {}: {}", path_str, e),
                    )
                })
                .unwrap();

            let mut syms = Vec::with_capacity(libs_syms[i].0.len());
            for sname in &libs_syms[i].0 {
                let c_name = CString::new(sname.as_str())
                    .map_err(|e| {
                        std::io::Error::new(
                            std::io::ErrorKind::Other,
                            format!("CString error: {}", e),
                        )
                    })
                    .unwrap();

                let sym = unsafe { libc::dlsym(handle.cast(), c_name.as_ptr()) };
                if sym == std::ptr::null_mut() {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!("Failed to load symbol {}: {}", sname, unsafe {
                            let err = libc::dlerror();
                            if err.is_null() {
                                "Unknown error".to_string()
                            } else {
                                CString::from_raw(err as *mut i8)
                                    .into_string()
                                    .unwrap_or_else(|_| "Invalid UTF8 in dlerror".to_string())
                            }
                        }),
                    ));
                }

                syms.push(Address::from_ptr(sym));
            }

            self.scm_libs
                .push((fbase, entrypoint, syms, libs_syms[i].1));
        }
        self.libraries_end = self.input.position() as usize;

        Ok(())
    }

    /// Read dylib section which holds all dynamic library references
    /// created from FFI API.
    ///
    /// Native extensions are in a separate section and are handled differently.
    //#[track_caller]
    fn read_dylibs(&mut self) -> std::io::Result<()> {
        let dylib_count = self.read16()? as usize;
        self.dylibs.reserve(dylib_count);
        for _ in 0..dylib_count {
            let fname_len = self.read32()? as usize;
            let fname = if fname_len == 0 {
                None
            } else {
                let mut buf = vec![0u8; fname_len];
                self.input.read_exact(&mut buf).unwrap();
                Some(String::from_utf8(buf).map_err(|e| {
                    std::io::Error::new(std::io::ErrorKind::Other, format!("UTF8 error: {}", e))
                })?)
            };
            let flags = self.read32()? as i32;

            let handle = unsafe {
                if let Some(fname) = &fname {
                    let c_fname = CString::new(fname.as_str())
                        .map_err(|e| {
                            std::io::Error::new(
                                std::io::ErrorKind::Other,
                                format!("CString error: {}", e),
                            )
                        })
                        .unwrap();
                    libc::dlopen(c_fname.as_ptr(), flags)
                } else {
                    libc::dlopen(std::ptr::null(), flags)
                }
            };

            if handle == std::ptr::null_mut() {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Failed to load dynamic library: {}", unsafe {
                        let err = libc::dlerror();
                        if err.is_null() {
                            "Unknown error".to_string()
                        } else {
                            CString::from_raw(err as *mut i8)
                                .into_string()
                                .unwrap_or_else(|_| "Invalid UTF8 in dlerror".to_string())
                        }
                    }),
                ));
            }

            let sym_count = self.read32()? as usize;

            let mut syms = vec![Address::ZERO; sym_count];

            let mut buf = Vec::with_capacity(64);
            for _ in 0..sym_count {
                let ix = self.read32()? as usize;
                let slen = self.read32()? as usize;

                buf.resize(slen, 0u8);
                self.input.read_exact(&mut buf).unwrap();
                let sname = String::from_utf8_lossy(&buf);

                let c_sname = CString::new(sname.as_ref())
                    .map_err(|e| {
                        std::io::Error::new(
                            std::io::ErrorKind::Other,
                            format!("CString error: {}", e),
                        )
                    })
                    .unwrap();

                let sym = unsafe { libc::dlsym(handle, c_sname.as_ptr()) };
                if sym == std::ptr::null_mut() {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!("Failed to load symbol: {}", unsafe {
                            let err = libc::dlerror();
                            if err.is_null() {
                                "Unknown error".to_string()
                            } else {
                                CString::from_raw(err as *mut i8)
                                    .into_string()
                                    .unwrap_or_else(|_| "Invalid UTF8 in dlerror".to_string())
                            }
                        }),
                    ));
                }
                buf.clear();

                *syms.get_mut(ix).unwrap() = Address::from_ptr(sym);
            }

            self.dylibs.push((Address::from_ptr(handle), syms));
        }

        Ok(())
    }

    /// Walk reference map in the image and pre-allocate memory
    /// for all objects.
    //#[track_caller]
    fn allocate_references(&mut self) -> std::io::Result<()> {
        self.reference_map_start = self.input.position() as usize;
        let nrefs = self.read32()? as usize;

        self.reference_map = vec![Value::undefined(); nrefs];
        for i in 0..nrefs {
            let next_object_start = self.read32()? as usize;
            let index = self.read32()? as usize;
            assert!(
                index == i,
                "Image reference map index mismatch: expected {}, got {}",
                i,
                index
            );
            let _instance_size = self.read32().unwrap();
            let header = self.read64().unwrap();
            // this allocates raw uninit memory for the object
            self.allocate_object(index, _instance_size as _, header)
                .unwrap();
            self.input.set_position(next_object_start as u64);
        }
        self.reference_map_end = self.input.position() as usize;

        Ok(())
    }

    /// Allocate a single object based on its type code.
    ///
    /// TODO: Actually use `_size`?
    //#[track_caller]
    fn allocate_object(&mut self, index: usize, _size: usize, header: u64) -> std::io::Result<()> {
        let hdr = ScmHeader { word: header };

        unsafe {
            let obj = match TypeCode8(hdr.type_bits() as _) {
                TypeCode8::DYNAMIC_STATE => {
                    let vt = VTableOf::<DynamicStateObject>::VT;
                    self.ctx.raw_allocate(
                        size_of::<DynamicStateObject>(),
                        align_of::<DynamicStateObject>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::FLUID => {
                    let vt = VTableOf::<Fluid>::VT;
                    self.ctx.raw_allocate(
                        size_of::<Fluid>(),
                        align_of::<Fluid>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::MODULE => {
                    let vt = VTableOf::<Module>::VT;
                    self.ctx.raw_allocate(
                        size_of::<Module>(),
                        align_of::<Module>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::VARIABLE => {
                    let vt = VTableOf::<Variable>::VT;
                    self.ctx.raw_allocate(
                        size_of::<Variable>(),
                        align_of::<Variable>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::BOX => {
                    let vt = VTableOf::<Boxed>::VT;
                    self.ctx.raw_allocate(
                        size_of::<Boxed>(),
                        align_of::<Boxed>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::HASHTABLE => {
                    let vt = VTableOf::<HashTable>::VT;
                    self.ctx.raw_allocate(
                        size_of::<HashTable>(),
                        align_of::<HashTable>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::PAIR => {
                    let vt = VTableOf::<Pair>::VT;
                    self.ctx.raw_allocate(
                        size_of::<Pair>(),
                        align_of::<Pair>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::NUMBER => {
                    let tc16 = TypeCode16(hdr.type_bits() as u16);
                    match tc16 {
                        TypeCode16::BIG => {
                            let ndigits = self.read32().unwrap();
                            let size = size_of::<BigInt>() + ndigits as usize * size_of::<Digit>();
                            let vt = BigInt::VT;

                            self.ctx.raw_allocate(
                                size,
                                align_of::<BigInt>(),
                                vt,
                                AllocationSemantics::Default,
                            )
                        }

                        TypeCode16::RATIONAL => {
                            let vt = VTableOf::<Rational>::VT;
                            self.ctx.raw_allocate(
                                size_of::<Rational>(),
                                align_of::<Rational>(),
                                vt,
                                AllocationSemantics::Default,
                            )
                        }

                        TypeCode16::COMPLEX => {
                            let vt = VTableOf::<Complex>::VT;
                            self.ctx.raw_allocate(
                                size_of::<Complex>(),
                                align_of::<Complex>(),
                                vt,
                                AllocationSemantics::Default,
                            )
                        }

                        _ => {
                            return Err(std::io::Error::new(
                                std::io::ErrorKind::Other,
                                format!("Unknown number type code: {:?}", tc16),
                            ));
                        }
                    }
                }

                TypeCode8::NATIVE_PROCEDURE => {
                    let vt = VTableOf::<NativeProc>::VT;
                    self.ctx.raw_allocate(
                        size_of::<NativeProc>(),
                        align_of::<NativeProc>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::CLOSURE => {
                    let vt = VTableOf::<Closure>::VT;
                    self.ctx.raw_allocate(
                        size_of::<Closure>(),
                        align_of::<Closure>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::STRINGBUF => {
                    let len = self.read32()? as usize;
                    let _is_mutable = self.read8()? != 0;
                    let is_wide = self.read8()? != 0;

                    let size = if is_wide {
                        size_of::<Stringbuf>() + len * size_of::<u32>()
                    } else {
                        size_of::<Stringbuf>() + len * size_of::<u8>()
                    };

                    let vt = Stringbuf::VT;

                    self.ctx.raw_allocate(
                        size,
                        align_of::<Stringbuf>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::STRING => self.ctx.raw_allocate(
                    size_of::<Str>(),
                    align_of::<Str>(),
                    VTableOf::<Str>::VT,
                    AllocationSemantics::Default,
                ),

                TypeCode8::SYMBOL => self.ctx.raw_allocate(
                    size_of::<Symbol>(),
                    align_of::<Symbol>(),
                    VTableOf::<Symbol>::VT,
                    AllocationSemantics::Default,
                ),

                TypeCode8::VECTOR => {
                    let len = self.read32()? as usize;
                    let vt = Vector::VT;
                    let size = size_of::<Vector>() + len * size_of::<Value>();
                    self.ctx.raw_allocate(
                        size,
                        align_of::<Vector>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::BYTEVECTOR => {
                    let len = self.read64()? as usize;
                    let vt = ByteVector::VT;
                    let size = size_of::<ByteVector>() + len * size_of::<u8>();
                    self.ctx.raw_allocate(
                        size,
                        align_of::<ByteVector>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::TUPLE => {
                    //let len = self.read32()? as usize;
                    let vt = Tuple::VT;
                    let len = TupleLengthBits::decode(header) as usize;
                    let size = size_of::<Tuple>() + len * size_of::<Value>();
                    self.ctx.raw_allocate(
                        size,
                        align_of::<Tuple>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::WEAKSET => {
                    let vt = VTableOf::<WeakSet>::VT;
                    self.ctx.raw_allocate(
                        size_of::<WeakSet>(),
                        align_of::<WeakSet>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::WEAK_MAPPING => {
                    let vt = VTableOf::<WeakMapping>::VT;
                    self.ctx.raw_allocate(
                        size_of::<WeakMapping>(),
                        align_of::<WeakMapping>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::WEAKTABLE => {
                    let vt = VTableOf::<WeakTable>::VT;
                    self.ctx.raw_allocate(
                        size_of::<WeakTable>(),
                        align_of::<WeakTable>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::CMARKS => {
                    let vt = VTableOf::<ContinuationMarks>::VT;
                    self.ctx.raw_allocate(
                        size_of::<ContinuationMarks>(),
                        align_of::<ContinuationMarks>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::POINTER => {
                    let vt = VTableOf::<Pointer>::VT;
                    self.ctx.raw_allocate(
                        size_of::<Pointer>(),
                        align_of::<Pointer>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::CIF => {
                    let vt = VTableOf::<CIF>::VT;
                    self.ctx.raw_allocate(
                        size_of::<CIF>(),
                        align_of::<CIF>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::SYNTAX => {
                    let vt = VTableOf::<Syntax>::VT;
                    self.ctx.raw_allocate(
                        size_of::<Syntax>(),
                        align_of::<Syntax>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                TypeCode8::SYNCLO => {
                    let vt = VTableOf::<SyntaxTransformer>::VT;
                    self.ctx.raw_allocate(
                        size_of::<SyntaxTransformer>(),
                        align_of::<SyntaxTransformer>(),
                        vt,
                        AllocationSemantics::Default,
                    )
                }

                _ => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        format!(
                            "Unsupported type code in image: {:?}",
                            TypeCode8(hdr.type_bits() as _)
                        ),
                    ));
                }
            };

            obj.to_address()
                .to_mut_ptr::<ScmHeader>()
                .write(ScmHeader { word: header });
            *self.reference_map.get_mut(index).unwrap() = val(obj);
        }

        Ok(())
    }

    pub fn read_value(&mut self) -> std::io::Result<Value<'gc>> {
        let typ = self.read8().unwrap();
        match typ {
            TYPE_TRUE => Ok(Value::new(true)),
            TYPE_FALSE => Ok(Value::new(false)),
            TYPE_UNDEFINED => Ok(Value::undefined()),
            TYPE_NULL => Ok(Value::null()),
            TYPE_EOF => Ok(Value::eof()),
            TYPE_CHAR => {
                let c = self.read32().unwrap();
                Ok(Value::from_char(std::char::from_u32(c).ok_or_else(
                    || {
                        std::io::Error::new(
                            std::io::ErrorKind::Other,
                            format!("Invalid char code: {}", c),
                        )
                    },
                )?))
            }

            TYPE_I32 => {
                let i = self.read32().unwrap();
                Ok(Value::new(i as i32))
            }

            TYPE_BWP => Ok(Value::bwp()),
            TYPE_IMMEDIATE => {
                let bits = self.read64().unwrap();
                Ok(Value::from_raw(bits))
            }

            TYPE_REF => {
                let index = self.read32()? as usize;
                Ok(self.reference_map.get(index).copied().unwrap())
            }

            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Unknown value type code: {}", typ),
            )),
        }
    }

    //#[track_caller]
    pub fn initialize_references(&mut self) -> std::io::Result<()> {
        self.input.set_position(self.reference_map_start as u64);
        let count = self.read32()? as usize;

        for i in 0..count {
            let next_object_start = self.read32()? as usize;
            let index = self.read32()? as usize;
            assert!(
                i == index,
                "Reference map index mismatch: expected {}, got {}",
                i,
                index
            );
            let _instance_size = self.read32().unwrap();
            let _header = self.read64().unwrap();
            let val = self.reference_map.get(index).copied().unwrap();
            let obj = val.as_cell_raw();

            if val.is::<DynamicStateObject>() {
                self.read_dynamic_state(obj).unwrap();
            } else if val.is::<Fluid>() {
                self.read_fluid(obj).unwrap();
            } else if val.is::<Module>() {
                self.read_module(obj).unwrap();
            } else if val.is::<Variable>() {
                self.read_variable(obj).unwrap();
            } else if val.is::<Boxed>() {
                self.read_box(obj).unwrap();
            } else if val.is::<HashTable>() {
                self.read_hashtable(obj).unwrap();
            } else if val.is::<Pair>() {
                self.read_pair(obj).unwrap();
            } else if val.is::<BigInt>() {
                self.read_bigint(obj).unwrap();
            } else if val.is::<Complex>() {
                self.read_complex(obj).unwrap();
            } else if val.is::<Rational>() {
                self.read_rational(obj).unwrap();
            } else if val.is::<NativeProc>() {
                self.read_native_proc(obj).unwrap();
            } else if val.is::<Closure>() {
                self.read_closure(obj).unwrap();
            } else if val.is::<Stringbuf>() {
                self.read_stringbuf(obj).unwrap();
            } else if val.is::<Str>() {
                self.read_string(obj).unwrap();
            } else if val.is::<Symbol>() {
                self.read_symbol(obj).unwrap();
            } else if val.is::<Vector>() {
                self.read_vector(obj).unwrap();
            } else if val.is::<ByteVector>() {
                self.read_bytevector(obj).unwrap();
            } else if val.is::<Tuple>() {
                self.read_tuple(obj).unwrap();
            } else if val.is::<WeakSet>() {
                self.read_weakset(obj).unwrap();
            } else if val.is::<WeakTable>() {
                self.read_weaktable(obj).unwrap();
            } else if val.is::<WeakMapping>() {
                self.read_weakmapping(obj).unwrap();
            } else if val.is::<ContinuationMarks>() {
                self.read_cmarks(obj).unwrap();
            } else if val.is::<Pointer>() {
                self.read_pointer(obj).unwrap();
            } else if val.is::<CIF>() {
                self.read_cif(obj).unwrap();
            } else if val.is::<Syntax>() {
                self.read_syntax(obj).unwrap();
            } else if val.is::<SyntaxTransformer>() {
                self.read_syntax_transformer(obj).unwrap();
            } else {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Unsupported object type during initialization: {:?}", val),
                ));
            }

            assert_eq!(
                self.input.position() as usize,
                next_object_start,
                "Object initialization did not read expected number of bytes: expected {}, got {}",
                next_object_start,
                self.input.position() as usize
            );
        }

        Ok(())
    }

    pub fn read_pair(&mut self, obj: GCObject) -> std::io::Result<()> {
        let car = self.read_value().unwrap();
        let cdr = self.read_value().unwrap();

        unsafe {
            let pair = obj.to_address().as_mut_ref::<Pair>();
            pair.car = Lock::new(car);
            pair.cdr = Lock::new(cdr);
        }

        Ok(())
    }

    pub fn read_vector(&mut self, obj: GCObject) -> std::io::Result<()> {
        let len = self.read32().unwrap();
        unsafe {
            let vector = obj.to_address().as_mut_ref::<Vector>();
            vector.length = len as _;
            for i in 0..len as usize {
                let val = self.read_value().unwrap();
                *vector[i].get_mut() = val;
            }
        }

        Ok(())
    }

    pub fn read_tuple(&mut self, obj: GCObject) -> std::io::Result<()> {
        unsafe {
            let tuple = obj.to_address().as_mut_ref::<Tuple>();

            for i in 0..tuple.len() as usize {
                let val = self.read_value().unwrap();
                tuple.data.as_mut_ptr().add(i).write(val.into());
            }
        }

        Ok(())
    }

    pub fn read_bytevector(&mut self, obj: GCObject) -> std::io::Result<()> {
        let len = self.read64().unwrap();
        unsafe {
            let bvec = obj.to_address().as_mut_ref::<ByteVector>();
            bvec.len = len as _;
            bvec.contents = obj.to_address().add(size_of::<ByteVector>());
            let data_ptr = bvec.contents().to_mut_ptr::<u8>();
            self.input
                .read_exact(std::slice::from_raw_parts_mut(data_ptr, len as usize))
                .unwrap();
        }

        Ok(())
    }

    pub fn read_stringbuf(&mut self, obj: GCObject) -> std::io::Result<()> {
        let len = self.read32().unwrap();
        let is_mutable = self.read8()? != 0;
        let _is_wide = self.read8()? != 0;
        unsafe {
            let strbuf = obj.to_address().as_mut_ref::<Stringbuf>();
            strbuf.length = len as _;
            if strbuf.is_wide() {
                let buf = strbuf.wide_chars_mut();
                for i in 0..len as usize {
                    let ch = self.read32().unwrap();
                    buf[i] = char::from_u32_unchecked(ch);
                }
            } else {
                let buf = strbuf.chars_mut();
                for i in 0..len as usize {
                    let ch = self.read8().unwrap();
                    buf[i] = ch;
                }
            }

            strbuf.set_mutable(is_mutable);
        }

        Ok(())
    }

    pub fn read_string(&mut self, obj: GCObject) -> std::io::Result<()> {
        let start = self.read32().unwrap();
        let len = self.read32().unwrap();
        let strbuf = unsafe { self.read_value()?.downcast_unchecked::<Stringbuf>() };

        unsafe {
            let string = obj.to_address().as_mut_ref::<Str>();
            string.stringbuf = Lock::new(strbuf);
            string.start.set(start as _);
            string.length = len as _;
        }

        Ok(())
    }

    pub fn read_symbol(&mut self, obj: GCObject) -> std::io::Result<()> {
        let stringbuf = unsafe { self.read_value()?.downcast_unchecked::<Stringbuf>() };

        unsafe {
            let symbol = obj.to_address().as_mut_ref::<Symbol>();
            symbol.stringbuf = stringbuf;
        }

        Ok(())
    }

    pub fn read_bigint(&mut self, obj: GCObject) -> std::io::Result<()> {
        let _ndigits = self.read32().unwrap();

        unsafe {
            let bigint = obj.to_address().as_mut_ref::<BigInt>();

            for i in 0..bigint.count() as usize {
                let digit = self.read64().unwrap();
                bigint[i] = digit;
            }
        }

        Ok(())
    }

    pub fn read_complex(&mut self, obj: GCObject) -> std::io::Result<()> {
        let real = self.read_value().unwrap();
        let imag = self.read_value().unwrap();

        unsafe {
            let complex = obj.to_address().as_mut_ref::<Complex>();
            complex.real = real.number().unwrap();
            complex.imag = imag.number().unwrap();
        }

        Ok(())
    }

    pub fn read_rational(&mut self, obj: GCObject) -> std::io::Result<()> {
        let numerator = self.read_value().unwrap();
        let denominator = self.read_value().unwrap();

        unsafe {
            let rational = obj.to_address().as_mut_ref::<Rational>();
            rational.numerator = numerator.number().unwrap();
            rational.denominator = denominator.number().unwrap();
        }

        Ok(())
    }

    pub fn read_box(&mut self, obj: GCObject) -> std::io::Result<()> {
        let value = self.read_value().unwrap();

        unsafe {
            let bx = obj.to_address().as_mut_ref::<Boxed>();
            bx.val = value;
        }

        Ok(())
    }

    pub fn read_module(&mut self, obj: GCObject) -> std::io::Result<()> {
        let obarray = unsafe { self.read_value()?.downcast_unchecked::<HashTable>() };
        let uses = self.read_value().unwrap();
        let binder = self.read_value().unwrap();
        let declarative = self.read8()? != 0;
        let transformer = self.read_value().unwrap();
        let name = self.read_value().unwrap();
        let version = self.read_value().unwrap();

        let kind = match self.read8()? {
            0 => ModuleKind::Directory,
            1 => ModuleKind::Interface,
            2 => ModuleKind::CustomInterface,
            _ => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "Invalid module kind",
                ));
            }
        };
        unsafe {
            let import_obarray = self.read_value()?.downcast_unchecked::<HashTable>();
            let submodules = self.read_value()?.downcast_unchecked::<HashTable>();
            let filename = self.read_value().unwrap();
            let public_interface = if self.read8()? != 0 {
                Some(self.read_value()?.downcast_unchecked::<Module>())
            } else {
                None
            };

            let next_unique_id = self.read64().unwrap();
            let replacements = self.read_value().unwrap();
            let inlinable_exports = self.read_value().unwrap();
            let environment = self.read_value().unwrap();

            let module = obj.to_address().as_mut_ref::<Module>();
            module.obarray = obarray.into();
            module.uses = uses.into();
            module.binder = binder;
            module.declarative = declarative.into();
            module.transformer = transformer;
            module.name = name.into();
            module.version = version.into();
            module.kind = kind.into();
            module.import_obarray = import_obarray;
            module.submodules = submodules;
            module.filename = filename.into();
            module.public_interface = public_interface.into();
            module.next_unique_id = AtomicUsize::new(next_unique_id as _);
            module.replacements = replacements.into();
            module.inlinable_exports = inlinable_exports.into();
            module.environment = environment.into();
        }

        Ok(())
    }

    pub fn read_variable(&mut self, obj: GCObject) -> std::io::Result<()> {
        let value = self.read_value().unwrap();

        unsafe {
            let var = obj.to_address().as_mut_ref::<Variable>();
            var.value = Lock::new(value);
        }

        Ok(())
    }

    pub fn read_hashtable(&mut self, obj: GCObject) -> std::io::Result<()> {
        unsafe {
            let typ = match self.read8()? {
                0 => HashTableType::Eq,
                1 => HashTableType::Eqv,
                2 => HashTableType::Equal,
                3 => HashTableType::String,
                4 => HashTableType::Generic(self.read_value()?),
                _ => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::Other,
                        "Invalid hashtable type",
                    ));
                }
            };

            let count = self.read32()? as usize;
            let mut kvs = Vec::with_capacity(count);
            for _ in 0..count {
                let key = self.read_value().unwrap();
                let value = self.read_value().unwrap();
                kvs.push((key, value));
            }

            HashTable::at_object(self.ctx, obj, typ, kvs);
        }

        Ok(())
    }

    pub fn read_weaktable(&mut self, obj: GCObject) -> std::io::Result<()> {
        unsafe {
            let count = self.read32()? as usize;
            let mut entries = Vec::with_capacity(count);
            for _ in 0..count {
                let key = self.read_value().unwrap();
                let value = self.read_value().unwrap();
                entries.push((key, value));
            }

            WeakTable::at_object(self.ctx, obj, entries);
        }

        Ok(())
    }

    pub fn read_weakmapping(&mut self, obj: GCObject) -> std::io::Result<()> {
        unsafe {
            let key = self.read_value().unwrap();
            let value = self.read_value().unwrap();

            let wm = obj.to_address().as_mut_ref::<WeakMapping>();
            wm._key = key;
            wm._value = value;
        }

        Ok(())
    }

    pub fn read_weakset(&mut self, _obj: GCObject) -> std::io::Result<()> {
        let count = self.read32()? as usize;
        let mut entries = Vec::with_capacity(count);
        for _ in 0..count {
            let value = self.read_value()?;
            let hash = self.read64()?;
            entries.push(WeakEntry { hash, value });
        }

        let entries = Array::from_iter(*self.ctx, entries.into_iter().map(Lock::new));

        let size = self.read32()? as usize;
        let n_items = self.read32()? as usize;
        let lower = self.read32()? as usize;
        let upper = self.read32()? as usize;
        let size_index = self.read32()? as usize;
        let min_size_index = self.read32()? as usize;

        let inner = WeakSetInner {
            entries: Lock::new(entries),
            size: Cell::new(size),
            n_items: Cell::new(n_items),
            lower: Cell::new(lower),
            upper: Cell::new(upper),
            size_index: Cell::new(size_index),
            min_size_index: Cell::new(min_size_index),
        };

        unsafe {
            let ws = _obj.to_address().as_mut_ref::<WeakSet>();
            ws.inner = Monitor::new(inner);
        }

        Ok(())
    }

    pub fn read_syntax(&mut self, obj: GCObject) -> std::io::Result<()> {
        let expr = self.read_value().unwrap();
        let wrap = self.read_value().unwrap();
        let module = self.read_value().unwrap();
        let source = self.read_value().unwrap();

        unsafe {
            let syntax = obj.to_address().as_mut_ref::<Syntax>();
            syntax.expr = expr;
            syntax.wrap = wrap;
            syntax.module = module;
            syntax.source = source;
        }

        Ok(())
    }

    pub fn read_syntax_transformer(&mut self, obj: GCObject) -> std::io::Result<()> {
        let name = self.read_value().unwrap();
        let typ = self.read_value().unwrap();
        let binding = self.read_value().unwrap();

        unsafe {
            let st = obj.to_address().as_mut_ref::<SyntaxTransformer>();
            st.name = name;
            st.typ = typ;
            st.binding = binding;
        }

        Ok(())
    }

    pub fn read_fluid(&mut self, obj: GCObject) -> std::io::Result<()> {
        let value = self.read_value().unwrap();

        unsafe {
            let fluid = obj.to_address().as_mut_ref::<Fluid>();

            fluid.value = value;
        }

        Ok(())
    }

    pub fn read_dynamic_state(&mut self, obj: GCObject) -> std::io::Result<()> {
        let saved = self.read_value().unwrap();
        unsafe {
            let ds = obj.to_address().as_mut_ref::<DynamicStateObject>();
            ds.saved = saved;
        }

        Ok(())
    }

    pub fn read_native_proc(&mut self, obj: GCObject) -> std::io::Result<()> {
        let native_index = self.read32()? as usize;

        unsafe {
            let np = obj.to_address().as_mut_ref::<NativeProc>();
            np.proc = self.native_procedures[native_index];
        }

        Ok(())
    }

    pub fn read_closure(&mut self, obj: GCObject) -> std::io::Result<()> {
        let meta = self.read_value().unwrap();
        let free = self.read_value().unwrap();

        let typ = self.read8().unwrap();

        if typ == TYPE_CLOSURE_NATIVE_PROC {
            let tramp = get_trampoline_from_scheme();
            unsafe {
                let closure = obj.to_address().as_mut_ref::<Closure>();
                closure.meta = meta.into();
                closure.free = free;
                closure.code = tramp;
                closure.direct = Address::ZERO;
            }
        } else if typ == TYPE_CLOSURE_NATIVE_CONT {
            let tramp = get_cont_trampoline_from_scheme();
            unsafe {
                let closure = obj.to_address().as_mut_ref::<Closure>();
                closure.meta = meta.into();
                closure.free = free;
                closure.code = tramp;
                closure.direct = Address::ZERO;
            }
        } else if typ == TYPE_CLOSURE_SCM {
            let lib_index = self.read16()? as usize;
            let sym_index = self.read32()? as usize;
            let lib = &self.scm_libs[lib_index];
            let sym_addr = lib.2[sym_index];

            unsafe {
                let closure = obj.to_address().as_mut_ref::<Closure>();
                closure.meta = meta.into();
                closure.free = free;
                closure.code = sym_addr;
                closure.direct = Address::ZERO;
            }
        } else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Invalid closure type",
            ));
        }

        Ok(())
    }

    pub fn read_pointer(&mut self, obj: GCObject) -> std::io::Result<()> {
        let typ = self.read8().unwrap();

        if typ == TYPE_POINTER_NULL {
            unsafe {
                let pointer = obj.to_address().as_mut_ref::<Pointer>();
                pointer.value = Address::ZERO;
            }

            Ok(())
        } else if typ == TYPE_POINTER_DYLIB {
            let lib_index = self.read16()? as usize;
            let offset = self.read32()? as i32 as isize;
            let sym_index = self.read32()? as usize;

            let lib = &self.dylibs[lib_index];
            let sym_addr = lib.1[sym_index];
            unsafe {
                let pointer = obj.to_address().as_mut_ref::<Pointer>();
                pointer.value = sym_addr + offset;
            }

            Ok(())
        } else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "Invalid pointer type",
            ));
        }
    }

    pub fn read_cif(&mut self, obj: GCObject) -> std::io::Result<()> {
        let _nargs = self.read32().unwrap();
        let variadic = self.read8()? != 0;
        let args = self.read_value().unwrap();
        let ret = self.read_value().unwrap();

        crate::runtime::vm::ffi::make_cif_at(self.ctx, obj, args, ret, variadic).map_err(|err| {
            std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Failed to create CIF: {}", err),
            )
        })
    }

    pub fn read_cmarks(&mut self, obj: GCObject) -> std::io::Result<()> {
        let marks = self.read_value().unwrap();
        unsafe {
            obj.to_address().as_mut_ref::<ContinuationMarks>().cmarks = marks;
        }

        Ok(())
    }

    pub fn read_globals(&mut self) -> std::io::Result<()> {
        VM_GLOBALS
            .set(crate::rsgc::Global::new(Globals::undefined()))
            .unwrap_or_else(|_| panic!("VM_GLOBALS already set"));
        self.ctx.globals().for_each_value(|global| {
            let val = self.read_value().unwrap();
            global.set(val);
        });

        Ok(())
    }
}

fn val<'gc>(x: GCObject) -> Value<'gc> {
    Value::from_raw(x.to_address().as_usize() as _)
}
