//! Collection of loaded shared libraries

use mmtk::util::metadata::side_metadata::global_side_metadata_vm_base_address;

use crate::rsgc::{mmtk::util::Address, sync::monitor::Monitor, Global, Trace};
use crate::runtime::{
    value::Value,
    vm::load::artifact::{LoadArtifact, LoadArtifactKind},
    Context,
};
use std::{ffi::OsStr, mem::MaybeUninit, sync::LazyLock};

pub struct SchemeLibrary<'gc> {
    pub library: *mut (),
    pub path: std::path::PathBuf,
    pub entrypoint: Value<'gc>,
    pub init_fn: extern "C-unwind" fn(Context<'gc>, usize) -> Value<'gc>,
    pub fbase: Address,

    pub globals: &'static [*mut Value<'static>],
}

unsafe impl<'gc> Trace for SchemeLibrary<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        unsafe {
            for global in self.globals.iter() {
                visitor.trace(&mut **global);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> SchemeLibrary<'gc> {
    pub fn load(
        ctx: Context<'gc>,
        path: impl AsRef<OsStr>,
        initialize: bool,
    ) -> std::io::Result<Self> {
        unsafe {
            let path = path.as_ref();

            let cpath = std::ffi::CString::new(path.as_encoded_bytes()).unwrap();
            let lib = libc::dlopen(cpath.as_ptr(), libc::RTLD_NOW | libc::RTLD_LOCAL);
            if lib.is_null() {
                let err = libc::dlerror();
                let err_str = if err.is_null() {
                    "Unknown error".to_string()
                } else {
                    std::ffi::CStr::from_ptr(err).to_string_lossy().into_owned()
                };
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Failed to load library {:?}: {}", path, err_str),
                ));
            }
            // get CAPY_GLOBALS
            let sym = libc::dlsym(lib, b"CAPY_GLOBALS\0".as_ptr() as *const libc::c_char);
            if sym.is_null() {
                let err = libc::dlerror();
                let err_str = if err.is_null() {
                    "Unknown dlsym error".to_string()
                } else {
                    std::ffi::CStr::from_ptr(err).to_string_lossy().into_owned()
                };
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Failed to find symbol CAPY_GLOBALS: {}", err_str),
                ));
            }
            let raw_globals = sym as *const *mut Value;

            // get CAPY_GLOBALS_LEN
            let sym = libc::dlsym(lib, b"CAPY_GLOBALS_LEN\0".as_ptr() as *const libc::c_char);
            if sym.is_null() {
                let err = libc::dlerror();
                let err_str = if err.is_null() {
                    "Unknown dlsym error".to_string()
                } else {
                    std::ffi::CStr::from_ptr(err).to_string_lossy().into_owned()
                };
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Failed to find symbol CAPY_GLOBALS_LEN: {}", err_str),
                ));
            }
            let globals_len_ptr = sym as *const u32;
            let globals_len = *globals_len_ptr as usize;

            let globals_slice = std::slice::from_raw_parts(raw_globals, globals_len);
            let globals: &'static [*mut Value] = std::mem::transmute(globals_slice);

            // get capy_module_init
            let sym = libc::dlsym(lib, b"capy_module_init\0".as_ptr() as *const libc::c_char);
            if sym.is_null() {
                let err = libc::dlerror();
                let err_str = if err.is_null() {
                    "Unknown dlsym error".to_string()
                } else {
                    std::ffi::CStr::from_ptr(err).to_string_lossy().into_owned()
                };
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Failed to find symbol capy_module_init: {}", err_str),
                ));
            }
            let module_init: extern "C-unwind" fn(Context<'gc>, usize) -> Value<'gc> =
                std::mem::transmute(sym);

            let mut dladdr: MaybeUninit<libc::Dl_info> = MaybeUninit::uninit();
            let res = libc::dladdr(module_init as *const libc::c_void, dladdr.as_mut_ptr());
            if res == 0 {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("dladdr failed for capy_module_init"),
                ));
            }

            let dladdr = dladdr.assume_init();
            let fbase = Address::from_ptr(dladdr.dli_fbase);

            let entrypoint = if initialize {
                module_init(ctx, global_side_metadata_vm_base_address().as_usize())
            } else {
                Value::new(false)
            };

            Ok(Self {
                path: std::path::PathBuf::from(path),
                library: lib.cast(),
                init_fn: module_init,
                fbase,
                globals,
                entrypoint,
            })
        }
    }

    pub fn for_each_global<F>(&self, mut f: F)
    where
        F: FnMut(&'static mut Value<'static>),
    {
        unsafe {
            for global in self.globals.iter() {
                f(&mut **global);
            }
        }
    }
}
pub enum LoadedLibrary<'gc> {
    SharedObject(SchemeLibrary<'gc>),
}

unsafe impl<'gc> Trace for LoadedLibrary<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        match self {
            Self::SharedObject(lib) => unsafe { lib.trace(visitor) },
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        match self {
            Self::SharedObject(lib) => unsafe { lib.process_weak_refs(weak_processor) },
        }
    }
}

impl<'gc> LoadedLibrary<'gc> {
    fn load(
        ctx: Context<'gc>,
        artifact: &LoadArtifact,
        initialize: bool,
    ) -> std::io::Result<(Self, Value<'gc>)> {
        match artifact.kind {
            LoadArtifactKind::SharedObject => {
                let lib = SchemeLibrary::load(ctx, &artifact.path, initialize)?;
                let entrypoint = lib.entrypoint;
                Ok((Self::SharedObject(lib), entrypoint))
            }
            LoadArtifactKind::StaticModuleObject
            | LoadArtifactKind::StaticArchive
            | LoadArtifactKind::StaticExecutable => Err(std::io::Error::new(
                std::io::ErrorKind::Unsupported,
                format!(
                    "artifact kind {:?} cannot be loaded with the dynamic library loader",
                    artifact.kind
                ),
            )),
        }
    }
}

pub struct LibraryCollection<'gc> {
    pub libs: Monitor<Vec<LoadedLibrary<'gc>>>,
}

unsafe impl<'gc> Trace for LibraryCollection<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        unsafe {
            self.libs.get_mut().trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> LibraryCollection<'gc> {
    pub fn new() -> Self {
        Self {
            libs: Monitor::new(Vec::with_capacity(2)),
        }
    }

    pub(crate) fn load(
        &self,
        artifact: &LoadArtifact,
        ctx: Context<'gc>,
    ) -> std::io::Result<Value<'gc>> {
        let (lib, entrypoint) = LoadedLibrary::load(ctx, artifact, true)?;
        self.libs.lock().push(lib);
        Ok(entrypoint)
    }

    #[allow(dead_code)]
    pub(crate) fn load_and_get_fbase(
        &self,
        artifact: &LoadArtifact,
        ctx: Context<'gc>,
    ) -> std::io::Result<(Address, *mut (), Value<'gc>)> {
        match artifact.kind {
            LoadArtifactKind::SharedObject => {
                let lib = SchemeLibrary::load(ctx, &artifact.path, false)?;
                let fbase = lib.fbase;
                let handle = lib.library;
                let entrypoint = lib.entrypoint;
                self.libs.lock().push(LoadedLibrary::SharedObject(lib));
                Ok((fbase, handle, entrypoint))
            }
            LoadArtifactKind::StaticModuleObject
            | LoadArtifactKind::StaticArchive
            | LoadArtifactKind::StaticExecutable => Err(std::io::Error::new(
                std::io::ErrorKind::Unsupported,
                format!(
                    "artifact kind {:?} does not have a dynamic library base address",
                    artifact.kind
                ),
            )),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn for_each_library<F>(&self, mut f: F)
    where
        F: FnMut(&LoadedLibrary<'gc>),
    {
        let libs = self.libs.lock();
        for lib in libs.iter() {
            f(lib);
        }
    }
}

pub static LIBRARY_COLLECTION: LazyLock<Global<crate::Rootable!(LibraryCollection<'_>)>> =
    LazyLock::new(|| Global::new(LibraryCollection::new()));
