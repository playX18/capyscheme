use std::path::{Path, PathBuf};

use super::policy::ExecutionPolicy;

pub(crate) const DYNLIB_EXTENSION: &str = std::env::consts::DLL_EXTENSION;
pub(crate) const COMPILED_SCHEME_EXTENSION: &str = "csc";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LoadArtifactKind {
    CompiledScheme,
    #[cfg(feature = "aot")]
    SharedObject,
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

pub(crate) fn artifact_kind_for_policy(policy: ExecutionPolicy) -> Option<LoadArtifactKind> {
    match policy {
        ExecutionPolicy::JIT => Some(LoadArtifactKind::CompiledScheme),
        #[cfg(feature = "aot")]
        ExecutionPolicy::AOT => Some(LoadArtifactKind::SharedObject),
    }
}

pub(crate) fn artifact_extension(kind: LoadArtifactKind) -> &'static str {
    match kind {
        LoadArtifactKind::CompiledScheme => COMPILED_SCHEME_EXTENSION,
        #[cfg(feature = "aot")]
        LoadArtifactKind::SharedObject => DYNLIB_EXTENSION,
    }
}

pub(crate) fn artifact_kind_for_path(path: &Path) -> Option<LoadArtifactKind> {
    match path.extension().and_then(|extension| extension.to_str()) {
        Some(extension) if extension == COMPILED_SCHEME_EXTENSION => {
            Some(LoadArtifactKind::CompiledScheme)
        }
        #[cfg(feature = "aot")]
        Some(extension) if extension == DYNLIB_EXTENSION => Some(LoadArtifactKind::SharedObject),

        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(feature = "aot")]
    fn aot_policy_selects_shared_object_artifacts() {
        assert_eq!(
            artifact_kind_for_policy(ExecutionPolicy::AOT),
            Some(LoadArtifactKind::SharedObject)
        );
    }

    #[test]
    fn jit_policy_selects_compiled_scheme_artifacts() {
        assert_eq!(
            artifact_kind_for_policy(ExecutionPolicy::JIT),
            Some(LoadArtifactKind::CompiledScheme)
        );
        assert_eq!(
            artifact_extension(LoadArtifactKind::CompiledScheme),
            COMPILED_SCHEME_EXTENSION
        );
    }

    #[test]
    fn csc_paths_are_compiled_scheme_artifacts() {
        assert_eq!(
            artifact_kind_for_path(Path::new("library.csc")),
            Some(LoadArtifactKind::CompiledScheme)
        );
    }

    #[test]
    #[cfg(not(feature = "aot"))]
    fn shared_objects_are_not_scheme_artifacts_without_aot() {
        let path = format!("library.{DYNLIB_EXTENSION}");
        assert_eq!(artifact_kind_for_path(Path::new(&path)), None);
    }

    #[test]
    #[cfg(feature = "aot")]
    fn shared_objects_are_scheme_artifacts_with_aot() {
        let path = format!("library.{DYNLIB_EXTENSION}");
        assert_eq!(
            artifact_kind_for_path(Path::new(&path)),
            Some(LoadArtifactKind::SharedObject)
        );
    }
}
