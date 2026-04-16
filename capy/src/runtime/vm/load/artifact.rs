use std::path::{Path, PathBuf};

use super::policy::ExecutionPolicy;

pub(crate) const CPS_SSA_EXTENSION: &str = "cscm";
pub(crate) const DYNLIB_EXTENSION: &str = std::env::consts::DLL_EXTENSION;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum LoadArtifactKind {
    SharedObject,
    CpsSsa,
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
        ExecutionPolicy::JIT => LoadArtifactKind::CpsSsa,
    }
}

pub(crate) fn artifact_extension(kind: LoadArtifactKind) -> &'static str {
    match kind {
        LoadArtifactKind::SharedObject => DYNLIB_EXTENSION,
        LoadArtifactKind::CpsSsa => CPS_SSA_EXTENSION,
    }
}

pub(crate) fn artifact_kind_for_path(path: &Path) -> Option<LoadArtifactKind> {
    match path.extension().and_then(|extension| extension.to_str()) {
        Some(extension) if extension == DYNLIB_EXTENSION => Some(LoadArtifactKind::SharedObject),
        Some(extension) if extension == CPS_SSA_EXTENSION => Some(LoadArtifactKind::CpsSsa),
        _ => None,
    }
}
