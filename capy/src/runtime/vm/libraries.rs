//! Collection of loaded shared libraries

use crate::runtime::{Context, value::Value};
use rsgc::{Global, Rootable, Trace, mmtk::util::Address, sync::monitor::Monitor};
use std::{ffi::OsStr, mem::MaybeUninit, sync::LazyLock};

pub struct SchemeLibrary<'gc> {
    pub library: *mut (),
    pub path: std::path::PathBuf,
    pub entrypoint: Value<'gc>,
    pub fbase: Address,

    pub globals: &'static [*mut Value<'static>],
}

unsafe impl<'gc> Trace for SchemeLibrary<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        unsafe {
            for global in self.globals.iter() {
                visitor.trace(&mut **global);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> SchemeLibrary<'gc> {
    pub fn load(ctx: Context<'gc>, path: impl AsRef<OsStr>) -> std::io::Result<Self> {
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
            let module_init: extern "C-unwind" fn(&Context<'gc>) -> Value<'gc> =
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

            let entrypoint = module_init(&ctx);

            Ok(Self {
                path: std::path::PathBuf::from(path),
                library: lib.cast(),
                fbase,
                globals,
                entrypoint,
            })
        }
    }
}

pub struct LibraryCollection<'gc> {
    pub libs: Monitor<Vec<SchemeLibrary<'gc>>>,
}

unsafe impl<'gc> Trace for LibraryCollection<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::Visitor) {
        unsafe {
            self.libs.get_mut().trace(visitor);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> LibraryCollection<'gc> {
    pub fn new() -> Self {
        Self {
            libs: Monitor::new(Vec::with_capacity(2)),
        }
    }

    pub fn load(&self, path: impl AsRef<OsStr>, ctx: Context<'gc>) -> std::io::Result<Value<'gc>> {
        let lib = SchemeLibrary::load(ctx, path)?;
        let entrypoint = lib.entrypoint;
        self.libs.lock().push(lib);
        Ok(entrypoint)
    }

    pub fn for_each_library<F>(&self, mut f: F)
    where
        F: FnMut(&SchemeLibrary<'gc>),
    {
        let libs = self.libs.lock();
        for lib in libs.iter() {
            f(lib);
        }
    }
}

pub static LIBRARY_COLLECTION: LazyLock<Global<Rootable!(LibraryCollection<'_>)>> =
    LazyLock::new(|| Global::new(LibraryCollection::new()));
