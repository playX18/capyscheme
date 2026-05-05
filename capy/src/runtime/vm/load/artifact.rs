use std::path::{Path, PathBuf};

use super::policy::ExecutionPolicy;

pub(crate) const DYNLIB_EXTENSION: &str = std::env::consts::DLL_EXTENSION;
pub(crate) const OBJECT_EXTENSION: &str = "o";
pub(crate) const STATIC_ARCHIVE_EXTENSION: &str = "a";
pub(crate) const EXECUTABLE_EXTENSION: &str = std::env::consts::EXE_EXTENSION;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LoadArtifactKind {
    SharedObject,
    StaticModuleObject,
    StaticArchive,
    StaticExecutable,
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
        ExecutionPolicy::AOT => LoadArtifactKind::SharedObject,
        _ => todo!(),
    }
}

pub(crate) fn artifact_extension(kind: LoadArtifactKind) -> &'static str {
    match kind {
        LoadArtifactKind::SharedObject => DYNLIB_EXTENSION,
        LoadArtifactKind::StaticModuleObject => OBJECT_EXTENSION,
        LoadArtifactKind::StaticArchive => STATIC_ARCHIVE_EXTENSION,
        LoadArtifactKind::StaticExecutable => EXECUTABLE_EXTENSION,
    }
}

pub(crate) fn artifact_kind_for_path(path: &Path) -> Option<LoadArtifactKind> {
    match path.extension().and_then(|extension| extension.to_str()) {
        Some(extension) if extension == DYNLIB_EXTENSION => Some(LoadArtifactKind::SharedObject),
        Some(extension) if extension == OBJECT_EXTENSION => {
            Some(LoadArtifactKind::StaticModuleObject)
        }
        Some(extension) if extension == STATIC_ARCHIVE_EXTENSION => {
            Some(LoadArtifactKind::StaticArchive)
        }
        Some(extension)
            if !EXECUTABLE_EXTENSION.is_empty() && extension == EXECUTABLE_EXTENSION =>
        {
            Some(LoadArtifactKind::StaticExecutable)
        }

        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classifies_static_artifact_extensions() {
        assert_eq!(
            artifact_kind_for_path(Path::new("module.o")),
            Some(LoadArtifactKind::StaticModuleObject)
        );
        assert_eq!(
            artifact_kind_for_path(Path::new("modules.a")),
            Some(LoadArtifactKind::StaticArchive)
        );
    }
}
