//! Collection of loaded compiled libraries

use crate::rsgc::{Global, Trace, sync::monitor::Monitor};
use crate::runtime::{
    code_image::{CompiledCodeImage, LoadedCodeImage},
    value::Value,
    vm::load::artifact::{LoadArtifact, LoadArtifactKind},
    Context,
};
use std::{fs, sync::LazyLock};

pub enum LoadedLibrary<'gc> {
    CodeImage(LoadedCodeImage<'gc>),
}

unsafe impl<'gc> Trace for LoadedLibrary<'gc> {
    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::Visitor) {
        match self {
            Self::CodeImage(image) => unsafe { image.trace(visitor) },
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::rsgc::WeakProcessor) {
        match self {
            Self::CodeImage(image) => unsafe { image.process_weak_refs(weak_processor) },
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
            LoadArtifactKind::SharedObject => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "shared-object Scheme artifacts are no longer loadable",
            )),
            LoadArtifactKind::FaslCode => {
                let bytes = fs::read(&artifact.path)?;
                let image = CompiledCodeImage::decode(&bytes)?;
                let image = LoadedCodeImage::load(ctx, image)?;
                let entrypoint = if initialize {
                    image.entry_closure()
                } else {
                    Value::new(false)
                };
                Ok((Self::CodeImage(image), entrypoint))
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
    use crate::runtime::{
        code_image::{CodeEntry, CODE_IMAGE_VERSION},
        value::Closure,
        Scheme,
    };
    use std::sync::Mutex;

    static TEST_LOCK: Mutex<()> = Mutex::new(());

    #[test]
    fn library_collection_loads_fasl_code_image_directly() {
        let _guard = TEST_LOCK
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let scm = Scheme::new_uninit();
        scm.enter(|ctx| {
            let image = CompiledCodeImage {
                version: CODE_IMAGE_VERSION,
                target_triple: target_lexicon::Triple::host().to_string(),
                constants_fasl: Vec::new(),
                code: vec![CodeEntry {
                    id: 1,
                    name: "entry".to_string(),
                    bytes: vec![0xc3],
                    entry_offset: 0,
                    arity: 0,
                    is_cont: false,
                    metadata_constant: None,
                }],
                data_slots: Vec::new(),
                relocations: Vec::new(),
                entry_code_id: 1,
                entry_is_cont: false,
            };
            let bytes = image.encode().expect("encode code image");
            let path = std::env::temp_dir()
                .join(format!("capy-test-code-image-{}.fasl", std::process::id()));
            fs::write(&path, bytes).expect("write code image");

            let artifact = LoadArtifact::new(LoadArtifactKind::FaslCode, &path);
            let libs = LibraryCollection::new();
            let entry = libs.load(&artifact, ctx).expect("load code image artifact");
            fs::remove_file(&path).expect("remove code image");

            assert!(entry.is::<Closure>());
            let mut loaded_count = 0;
            libs.for_each_library(|lib| {
                if matches!(lib, LoadedLibrary::CodeImage(_)) {
                    loaded_count += 1;
                }
            });
            assert_eq!(loaded_count, 1);
        });
    }
}
