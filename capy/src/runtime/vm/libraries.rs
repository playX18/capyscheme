//! Collection of loaded compiled libraries

use crate::rsgc::{Global, Trace, sync::monitor::Monitor};
use crate::runtime::{
    Context,
    fasl::FaslReader,
    value::Value,
    vm::load::artifact::{LoadArtifact, LoadArtifactKind},
};
use std::{fs, io::Cursor, sync::LazyLock};

pub enum Library<'gc> {
    Fasl(Value<'gc>),
}

unsafe impl<'gc> Trace for Library<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        match self {
            Self::Fasl(value) => unsafe { value.trace(visitor) },
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        match self {
            Self::Fasl(value) => unsafe { value.process_weak_refs(weak_processor) },
        }
    }
}

impl<'gc> Library<'gc> {
    fn load(
        ctx: Context<'gc>,
        artifact: &LoadArtifact,
        initialize: bool,
    ) -> std::io::Result<(Self, Value<'gc>)> {
        match artifact.kind {
            LoadArtifactKind::SharedObject => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "shared-object Scheme artifacts are no longer loadable",
            )),
            LoadArtifactKind::FaslCode => {
                let bytes = fs::read(&artifact.path)?;
                let value = FaslReader::new(ctx, Cursor::new(bytes)).read()?;
                let entrypoint = if initialize { value } else { Value::new(false) };
                Ok((Self::Fasl(value), entrypoint))
            }
        }
    }
}

pub struct LibraryCollection<'gc> {
    pub libs: Monitor<Vec<Library<'gc>>>,
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
        let (lib, entrypoint) = Library::load(ctx, artifact, true)?;
        self.libs.lock().push(lib);
        Ok(entrypoint)
    }

    #[allow(dead_code)]
    pub(crate) fn for_each_library<F>(&self, mut f: F)
    where
        F: FnMut(&Library<'gc>),
    {
        let libs = self.libs.lock();
        for lib in libs.iter() {
            f(lib);
        }
    }
}

impl<'gc> Default for LibraryCollection<'gc> {
    fn default() -> Self {
        Self::new()
    }
}

type RootedLibraryCollection = crate::Rootable!(LibraryCollection<'_>);

pub static LIBRARY_COLLECTION: LazyLock<Global<RootedLibraryCollection>> =
    LazyLock::new(|| Global::new(LibraryCollection::new()));

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::{
        Scheme,
        fasl::{CodeSpec, FaslCompression, FaslImage, FaslWriter, GraphCodeSpec, ProgramSpec},
        value::{Closure, Value},
    };
    use std::sync::Mutex;

    static TEST_LOCK: Mutex<()> = Mutex::new(());

    #[cfg(target_arch = "x86_64")]
    #[test]
    fn library_collection_loads_unified_fasl_value() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let mut bytes = Vec::new();
            let code = CodeSpec::new(&[0xc3], 0, 0, false, Value::new(false), &[]);
            let code_blocks = [GraphCodeSpec::new(0, code)];
            let program = ProgramSpec::new(1, &[], &code_blocks, 0, false);
            FaslWriter::new(ctx, &mut bytes)
                .write_image(FaslImage::Program(&program), FaslCompression::None)
                .expect("write unified FASL");
            let path = std::env::temp_dir().join(format!(
                "capy-test-unified-fasl-{}.fasl",
                std::process::id()
            ));
            fs::write(&path, bytes).expect("write unified FASL artifact");

            let artifact = LoadArtifact::new(LoadArtifactKind::FaslCode, &path);
            let libs = LibraryCollection::new();
            let entry = libs
                .load(&artifact, ctx)
                .expect("load unified FASL artifact");
            fs::remove_file(&path).expect("remove unified FASL artifact");

            assert!(entry.is::<Closure>());
            let mut loaded_count = 0;
            libs.for_each_library(|lib| {
                let Library::Fasl(value) = lib;
                loaded_count += 1;
                assert!(value.is::<Closure>());
            });
            assert_eq!(loaded_count, 1);
        });
    }
}
