//! Collection of loaded shared libraries

use std::{ffi::OsStr, sync::LazyLock};

use rsgc::{Global, Rootable, Trace, sync::monitor::Monitor};

use crate::runtime::{Context, value::Value};

pub struct SchemeLibrary<'gc> {
    pub library: libloading::Library,
    pub entrypoint: Value<'gc>,

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
    pub fn load(ctx: Context<'gc>, path: impl AsRef<OsStr>) -> Result<Self, libloading::Error> {
        unsafe {
            let lib = libloading::Library::new(path)?;

            let globals: libloading::Symbol<*const *mut Value> = lib.get(b"CAPY_GLOBALS\0")?;
            let globals_len: libloading::Symbol<*const u32> = lib.get(b"CAPY_GLOBALS_LEN\0")?;

            let globals: &'static [*mut Value] =
                std::slice::from_raw_parts(*globals, **globals_len as usize);

            let module_init: libloading::Symbol<extern "C-unwind" fn(&Context<'gc>) -> Value<'gc>> =
                lib.get(b"capy_module_init\0")?;

            let entrypoint = module_init(&ctx);

            Ok(Self {
                library: lib,
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

    pub fn load(
        &self,
        path: impl AsRef<OsStr>,
        ctx: Context<'gc>,
    ) -> Result<Value<'gc>, libloading::Error> {
        let lib = SchemeLibrary::load(ctx, path)?;
        let entrypoint = lib.entrypoint;
        self.libs.lock().push(lib);
        Ok(entrypoint)
    }
}

pub static LIBRARY_COLLECTION: LazyLock<Global<Rootable!(LibraryCollection<'_>)>> =
    LazyLock::new(|| Global::new(LibraryCollection::new()));
