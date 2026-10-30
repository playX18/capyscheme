//! Collection of loaded compiled libraries

use std::{fs, io::Cursor, sync::LazyLock};

use crate::heap::{Global, Trace, sync::monitor::Monitor};
use crate::runtime::{
    MutationContext,
    fasl::{Reader, fasl_payload},
    value::Value,
    vm::load::{
        artifact::{LoadArtifact, LoadArtifactKind},
        policy::get_fasl_load_options,
    },
};

pub enum Library<'gc> {
    Fasl(Value<'gc>),
}

// SAFETY: `gc` for `Library` upholds all trait invariants
unsafe impl<'gc> Trace for Library<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        match self {
            // SAFETY: Preconditions verified by the surrounding code
            Self::Fasl(value) => unsafe { value.trace(visitor) },
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
        match self {
            // SAFETY: Preconditions verified by the surrounding code
            Self::Fasl(value) => unsafe { value.process_weak_refs(weak_processor) },
        }
    }
}

impl<'gc> Library<'gc> {
    fn load(
        ctx: MutationContext<'gc>,
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
                Self::from_fasl(ctx, &bytes, initialize)
            }
        }
    }

    fn from_fasl(
        ctx: MutationContext<'gc>,
        bytes: &[u8],
        initialize: bool,
    ) -> std::io::Result<(Self, Value<'gc>)> {
        let value = Reader::new_with_options(
            ctx,
            Cursor::new(fasl_payload(bytes)),
            get_fasl_load_options(),
        )
        .read()?;
        let entrypoint = if initialize { value } else { Value::new(false) };
        Ok((Self::Fasl(value), entrypoint))
    }
}

pub struct LibraryCollection<'gc> {
    pub libs: Monitor<Vec<Library<'gc>>>,
}

// SAFETY: `gc` for `LibraryCollection` upholds all trait invariants
unsafe impl<'gc> Trace for LibraryCollection<'gc> {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut crate::heap::Visitor) {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            self.libs.get_mut().trace(visitor);
        }
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::heap::WeakProcessor) {
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
        ctx: MutationContext<'gc>,
    ) -> std::io::Result<Value<'gc>> {
        let (lib, entrypoint) = Library::load(ctx, artifact, true)?;
        self.libs.lock().push(lib);
        Ok(entrypoint)
    }

    /// Loads a FASL code image that is in memory, see `SourceJob::unsaved`.
    pub(crate) fn load_image(
        &self,
        bytes: &[u8],
        ctx: MutationContext<'gc>,
    ) -> std::io::Result<Value<'gc>> {
        let (lib, entrypoint) = Library::from_fasl(ctx, bytes, true)?;
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
