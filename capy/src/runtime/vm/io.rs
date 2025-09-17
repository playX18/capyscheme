use crate::{native_fn, runtime::prelude::*, static_symbols};

static_symbols!(
    SYM_INPUT = "input"
    SYM_OUTPUT = "output"
    SYM_ERROR = "error"
    SYM_BINARY = "binary"
    SYM_TEXTUAL = "textual"
);

native_fn!(
    register_io_fns:

    pub ("file-exists?") fn file_exists<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        let metadata = std::fs::metadata(path.to_string());
        nctx.return_(metadata.is_ok())
    }

    pub ("absolute-path-string?") fn is_absolute_path_string<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        let p = std::path::PathBuf::from(path.to_string());
        nctx.return_(p.is_absolute())
    }

    pub ("osdep/open-console") fn open_console<'gc>(nctx, mode: Value<'gc>) -> Value<'gc> {
        let fd = if mode == sym_input(nctx.ctx).into() {
            libc::STDIN_FILENO
        } else if mode == sym_output(nctx.ctx).into() {
            libc::STDOUT_FILENO
        } else if mode == sym_error(nctx.ctx).into() {
            libc::STDERR_FILENO
        } else {
            return nctx.wrong_argument_violation("osdep/open-console", "invalid mode, expected 'input, 'output, or 'error", Some(mode), Some(0), 1, &[mode]);
        };

        nctx.return_(Value::new(fd))
    }

    pub ("osdep/close-console") fn close_console<'gc>(nctx, _fd: i32) -> Value<'gc> {
        // We don't actually close the standard file descriptors.
        // Just return true to indicate success.
        nctx.return_(Value::new(true))
    }

);

pub fn init_io<'gc>(ctx: Context<'gc>) {
    register_io_fns(ctx);
}
