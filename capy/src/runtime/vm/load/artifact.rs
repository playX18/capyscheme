use std::path::{Path, PathBuf};

use super::policy::ExecutionPolicy;

pub(crate) const DYNLIB_EXTENSION: &str = std::env::consts::DLL_EXTENSION;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LoadArtifactKind {
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
        ExecutionPolicy::AOT => Some(LoadArtifactKind::SharedObject),
        ExecutionPolicy::JIT => None,
    }
}

pub(crate) fn artifact_extension(kind: LoadArtifactKind) -> &'static str {
    match kind {
        LoadArtifactKind::SharedObject => DYNLIB_EXTENSION,
    }
}

pub(crate) fn artifact_kind_for_path(path: &Path) -> Option<LoadArtifactKind> {
    match path.extension().and_then(|extension| extension.to_str()) {
        Some(extension) if extension == DYNLIB_EXTENSION => Some(LoadArtifactKind::SharedObject),

        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn aot_policy_selects_shared_object_artifacts() {
        assert_eq!(
            artifact_kind_for_policy(ExecutionPolicy::AOT),
            Some(LoadArtifactKind::SharedObject)
        );
    }

    #[test]
    fn jit_policy_selects_no_persistent_artifact() {
        assert_eq!(artifact_kind_for_policy(ExecutionPolicy::JIT), None);
    }
}
