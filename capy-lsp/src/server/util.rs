use std::{
    env,
    path::PathBuf,
    sync::{Mutex, MutexGuard},
};

use async_lsp::{ErrorCode, ResponseError};
use lsp_types::{InitializeParams, Position, Range, Url};

pub(super) fn workspace_root_from_initialize(params: &InitializeParams) -> Option<PathBuf> {
    if let Some(folder) = params
        .workspace_folders
        .as_ref()
        .and_then(|folders| folders.first())
    {
        if let Ok(path) = folder.uri.to_file_path() {
            return Some(path);
        }
    }

    #[allow(deprecated)]
    if let Some(uri) = &params.root_uri {
        if let Ok(path) = uri.to_file_path() {
            return Some(path);
        }
    }

    env::current_dir().ok()
}

pub(super) fn contains_position(range: Range, position: Position) -> bool {
    (position.line > range.start.line
        || (position.line == range.start.line && position.character >= range.start.character))
        && (position.line < range.end.line
            || (position.line == range.end.line && position.character <= range.end.character))
}

pub(super) fn file_name(uri: &Url) -> Option<String> {
    uri.to_file_path().ok().and_then(|path| {
        path.file_name()
            .and_then(|name| name.to_str())
            .map(str::to_owned)
    })
}

pub(super) fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<'_, T> {
    mutex
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

pub(super) fn io_error(err: std::io::Error) -> async_lsp::Error {
    ResponseError::new(ErrorCode::INTERNAL_ERROR, err).into()
}

pub(super) fn io_response_error(err: std::io::Error) -> ResponseError {
    ResponseError::new(ErrorCode::INTERNAL_ERROR, err)
}
