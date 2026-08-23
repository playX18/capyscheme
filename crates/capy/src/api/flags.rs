//! Flag introspection exports for the thin `capy`/`capyc` launchers
//! (`capy --help-flags`); pure data accessors, no VM initialization.

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

/// NUL-terminated `--cli-alias ENV_VAR` pairs, one per line. Parsed by the
/// pre-boot CLI flag table; the trailing NUL is part of the string.
#[unsafe(no_mangle)]
pub extern "C" fn capy_flags_cli_aliases() -> *const libc::c_char {
    static ALIASES: LazyLock<CString> = LazyLock::new(|| {
        let mut text = String::new();
        for (cli, env) in flags::cli_aliases() {
            text.push_str(cli);
            text.push(' ');
            text.push_str(env);
            text.push('\n');
        }
        CString::new(text).expect("alias names contain no NUL bytes")
    });
    ALIASES.as_ptr()
}
