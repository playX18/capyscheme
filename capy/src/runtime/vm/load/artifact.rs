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
        ExecutionPolicy::AOT => LoadArtifactKind::FaslCode,
        _ => todo!(),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn aot_policy_uses_fasl_code_artifacts() {
        assert_eq!(
            artifact_kind_for_policy(ExecutionPolicy::AOT),
            LoadArtifactKind::FaslCode
        );
    }

    #[test]
    fn fasl_code_artifact_extension_is_fasl() {
        assert_eq!(
            artifact_extension(LoadArtifactKind::FaslCode),
            FASL_CODE_EXTENSION
        );
    }

    #[test]
    fn artifact_kind_for_path_recognizes_fasl_code_images() {
        assert_eq!(
            artifact_kind_for_path(Path::new("library.fasl")),
            Some(LoadArtifactKind::FaslCode)
        );
    }
}
