//! Flag introspection exports for the thin `capy`/`capyc` launchers
//! (`capy --help-flags`), pure data accessors, no VM initialization.

use std::ffi::CString;
use std::sync::LazyLock;

use crate::utils::flags;

/// NUL-terminated, human-readable listing of every flag (`--help-flags`).
#[unsafe(no_mangle)]
pub extern "C" fn capy_flags_help() -> *const libc::c_char {
    static HELP: LazyLock<CString> = LazyLock::new(|| {
        CString::new(flags::help_text()).expect("help text contains no NUL bytes")
    });
    HELP.as_ptr()
}

/// NUL-terminated `--cli-alias ENV_VAR bool|value` triples, one per
/// line. Parsed by the pre-boot CLI flag table, the trailing NUL is part
/// of the string.
#[unsafe(no_mangle)]
pub extern "C" fn capy_flags_cli_aliases() -> *const libc::c_char {
    static ALIASES: LazyLock<CString> = LazyLock::new(|| {
        let mut text = String::new();
        for alias in flags::cli_aliases() {
            text.push_str(alias.cli);
            text.push(' ');
            text.push_str(alias.env);
            text.push(' ');
            text.push_str(if alias.is_bool { "bool" } else { "value" });
            text.push('\n');
        }
        CString::new(text).expect("alias names contain no NUL bytes")
    });
    ALIASES.as_ptr()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn capy_flags_set(
    name: *const libc::c_char,
    value: *const libc::c_char,
) -> libc::c_int {
    if name.is_null() || value.is_null() {
        return -1;
    }
    let (name, value) = unsafe {
        (
            std::ffi::CStr::from_ptr(name),
            std::ffi::CStr::from_ptr(value),
        )
    };
    let (Ok(name), Ok(value)) = (name.to_str(), value.to_str()) else {
        return -1;
    };
    if name.is_empty() || name.contains('=') {
        return -1;
    }
    unsafe { flags::set_var(name, value) };
    0
}
