//! Collection of loaded shared libraries

use crate::rsgc::{Global, Trace, mmtk::util::Address, sync::monitor::Monitor};
use crate::runtime::{
    Context,
    value::Value,
    vm::load::artifact::{LoadArtifact, LoadArtifactKind},
};
use std::{ffi::OsStr, mem::MaybeUninit, sync::LazyLock};

pub struct SchemeLibrary<'gc> {
    pub library: *mut (),
    pub path: std::path::PathBuf,
    pub entrypoint: Value<'gc>,
    pub init_fn: extern "C-unwind" fn(Context<'gc>) -> Value<'gc>,
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
            let module_init: extern "C-unwind" fn(Context<'gc>) -> Value<'gc> =
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
                module_init(ctx)
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compiler::{CompilationOptions, compile_cps_to_shared_object, compile_file},
        prelude::Closure,
        runtime::{self, Context, Scheme},
    };
    use std::{env, fs, path::PathBuf, sync::Mutex};
    use uuid::Uuid;

    static TEST_LOCK: Mutex<()> = Mutex::new(());

    struct TempDir {
        path: PathBuf,
    }

    impl TempDir {
        fn new() -> Self {
            let path = std::env::temp_dir().join(format!("capy-library-test-{}", Uuid::new_v4()));
            fs::create_dir_all(&path).unwrap();
            Self { path }
        }

        fn path(&self) -> &std::path::Path {
            &self.path
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.path);
        }
    }

    fn with_ctx(f: impl for<'gc> FnOnce(Context<'gc>)) {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            if crate::runtime::global::VM_GLOBALS.get().is_none() {
                runtime::init(ctx);
            }
            f(ctx);
        });
    }

    fn compile_sample_source<'gc>(
        ctx: Context<'gc>,
        source_dir: &std::path::Path,
    ) -> crate::cps::term::FuncRef<'gc> {
        let source = source_dir.join("sample.scm");
        fs::write(&source, "42\n").unwrap();
        compile_file(ctx, &source, None).unwrap()
    }

    fn configure_capy_library_search_path() {
        let release_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../target")
            .join(format!("{}-unknown-linux-gnu", std::env::consts::ARCH))
            .join("release");
        if release_dir.join("libcapy.so").exists() {
            unsafe {
                env::set_var("LIBRARY_PATH", &release_dir);
                env::set_var("LD_LIBRARY_PATH", &release_dir);
            }
        }
    }

    #[test]
    fn shared_object_loading_still_returns_a_closure() {
        with_ctx(|ctx| {
            configure_capy_library_search_path();
            let temp = TempDir::new();
            let cps = compile_sample_source(ctx, temp.path());
            let output = temp
                .path()
                .join(format!("sample.{}", std::env::consts::DLL_EXTENSION));
            compile_cps_to_shared_object(ctx, cps, CompilationOptions::default(), &output).unwrap();

            let collection = LibraryCollection::new();
            let artifact = LoadArtifact::new(LoadArtifactKind::SharedObject, &output);
            let entrypoint = collection.load(&artifact, ctx).unwrap();

            assert!(entrypoint.is::<Closure>());
        });
    }
}
