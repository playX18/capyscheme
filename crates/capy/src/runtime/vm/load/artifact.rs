use std::path::{Path, PathBuf};

use super::policy::ExecutionPolicy;

pub(crate) const DYNLIB_EXTENSION: &str = std::env::consts::DLL_EXTENSION;
pub(crate) const FASL_CODE_EXTENSION: &str = "fasl";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LoadArtifactKind {
    SharedObject,
    FaslCode,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LoadArtifact {
    pub(crate) kind: LoadArtifactKind,
    pub(crate) path: PathBuf,
}

impl LoadArtifact {
    pub(crate) fn new(kind: LoadArtifactKind, path: impl Into<PathBuf>) -> Self {
        Self {
            kind,
            path: path.into(),
        }
    }
}

pub(crate) fn artifact_kind_for_policy(policy: ExecutionPolicy) -> LoadArtifactKind {
    match policy {
        ExecutionPolicy::Aot => LoadArtifactKind::FaslCode,
        ExecutionPolicy::Jit => LoadArtifactKind::SharedObject,
    }
}

pub(crate) fn artifact_extension(kind: LoadArtifactKind) -> &'static str {
    match kind {
        LoadArtifactKind::SharedObject => DYNLIB_EXTENSION,
        LoadArtifactKind::FaslCode => FASL_CODE_EXTENSION,
    }
}

pub(crate) fn artifact_kind_for_path(path: &Path) -> Option<LoadArtifactKind> {
    match path.extension().and_then(|extension| extension.to_str()) {
        Some(extension) if extension == DYNLIB_EXTENSION => Some(LoadArtifactKind::SharedObject),
        Some(FASL_CODE_EXTENSION) => Some(LoadArtifactKind::FaslCode),

        _ => None,
    }
}

/// Whether the file at `path` starts with a FASL image
pub(crate) fn is_fasl_file(path: &Path) -> bool {
    use std::io::Read;

    const HEAD_BYTES: u64 = 4096;
    let Ok(file) = std::fs::File::open(path) else {
        return false;
    };
    let mut head = Vec::new();
    file.take(HEAD_BYTES).read_to_end(&mut head).is_ok()
        && crate::runtime::fasl::is_fasl_image(&head)
}
