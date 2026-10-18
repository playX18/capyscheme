use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Visitor, WeakProcessor};
use crate::prelude::*;
use crate::runtime::fasl::{Reader, Writer};
use crate::static_symbols;
use rustix::fd::AsRawFd;
use std::ffi::CString;
use std::sync::{
    Mutex,
    atomic::{AtomicU64, Ordering},
};
static_symbols!(
    SYM_INPUT = "input"
    SYM_OUTPUT = "output"
    SYM_ERROR = "error"
    SYM_BINARY = "binary"
    SYM_TEXTUAL = "textual"
);

static TERM_SIGWINCH_VERSION: AtomicU64 = AtomicU64::new(0);
static TERM_RAW_MODE_STATE: Mutex<Option<(i32, libc::termios)>> = Mutex::new(None);

extern "C" fn term_sigwinch_handler(_: libc::c_int) {
    TERM_SIGWINCH_VERSION.fetch_add(1, Ordering::Relaxed);
}

fn term_io_error<'gc>(
    call: &Call<'_, 'gc>,
    operation: IoOperation,
    who: &'static str,
    irritant: Value<'gc>,
) -> Value<'gc> {
    let err = std::io::Error::last_os_error();
    let msg = err.to_string();
    call.raise_io_error(err, operation, who, &msg, irritant)
}

// SAFETY: Caller must ensure preconditions are met (see fn docs)
unsafe fn open_tty_raw_fd() -> Result<i32, std::io::Error> {
    let path = CString::new("/dev/tty").expect("static tty path");
    // SAFETY: FFI: path is a valid NUL-terminated string with valid flags
    let fd = unsafe { libc::open(path.as_ptr(), libc::O_RDWR | libc::O_CLOEXEC) };
    if fd < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(fd)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IoOperation {
    Open,
    Read,
    Write,
    Seek,
    Close,
    Stat,
    Select,
}
#[scheme(path=capy)]
pub mod io_ops {

    #[scheme(name = "usleep")]
    pub fn usleep(microseconds: u64) -> bool {
        std::thread::sleep(std::time::Duration::from_micros(microseconds));
        true
    }

    #[scheme(name = "home-directory")]
    pub fn home_directory() -> Value<'gc> {
        match std::env::home_dir() {
            Some(buf) => {
                let s = buf.to_string_lossy();
                Str::new(ctx, &s, true).into()
            }
            None => call.raise_io_error(
                std::io::Error::new(std::io::ErrorKind::NotFound, "home directory not found"),
                IoOperation::Open,
                "home-directory",
                "home directory not found",
                Value::new(false),
            ),
        }
    }

    #[scheme(name = "gethostname")]
    pub fn gethostname() -> Value<'gc> {
        match hostname::get() {
            Ok(name_osstr) => {
                let name_str = name_osstr.to_string_lossy();
                Str::new(ctx, &name_str, true).into()
            }
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "gethostname",
                    &error,
                    Value::new(false),
                )
            }
        }
    }

    #[scheme(name = "dirname")]
    pub fn dirname(path: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        let p = std::path::PathBuf::from(path.to_string());
        let dir = p.parent().unwrap_or_else(|| std::path::Path::new(""));
        let s = dir.to_string_lossy();
        Str::new(ctx, &s, true)
    }

    #[scheme(name = "getenv")]
    pub fn getenv(var: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::env::var(var.to_string()) {
            Ok(s) => Str::new(ctx, &s, true).into(),
            Err(_) => Value::new(false),
        }
    }

    #[scheme(name = "directory-list")]
    pub fn directory_list(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        let p = std::path::PathBuf::from(path.to_string());

        match std::fs::read_dir(p) {
            Ok(entries) => {
                let mut ls = Value::null();
                for entry in entries.flatten() {
                    let name = entry.file_name().to_string_lossy().into_owned();
                    ls = Value::cons(ctx, Str::new(ctx, &name, true).into(), ls);
                }
                ls = ls.list_reverse(ctx);
                ls
            }
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "directory-list",
                    &error,
                    path.into(),
                )
            }
        }
    }

    #[scheme(name = "current-directory")]
    pub fn current_directory(path: Option<Gc<'gc, Str<'gc>>>) -> Value<'gc> {
        match path {
            Some(path) => match std::env::set_current_dir(path.to_string()) {
                Ok(()) => Str::new(ctx, path.to_string(), true).into(),
                Err(err) => {
                    let error = err.to_string();
                    call.raise_io_error(
                        err,
                        IoOperation::Open,
                        "current-directory",
                        &error,
                        path.into(),
                    )
                }
            },

            None => match std::env::current_dir() {
                Ok(buf) => {
                    let s = buf.to_string_lossy();
                    Str::new(ctx, &s, true).into()
                }
                Err(err) => {
                    let error = err.to_string();
                    call.raise_io_error(
                        err,
                        IoOperation::Open,
                        "current-directory",
                        &error,
                        Value::new(false),
                    )
                }
            },
        }
    }

    #[scheme(name = "create-directory")]
    pub fn create_directory(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::create_dir(path.to_string()) {
            Ok(()) => Value::new(true),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "create-directory",
                    &error,
                    path.into(),
                )
            }
        }
    }

    #[scheme(name = "create-directory*")]
    pub fn create_directory_all(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::create_dir_all(path.to_string()) {
            Ok(()) => Value::new(true),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "create-directory*",
                    &error,
                    path.into(),
                )
            }
        }
    }

    #[scheme(name = "delete-file")]
    pub fn delete_file(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::remove_file(path.to_string()) {
            Ok(()) => Value::new(true),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(err, IoOperation::Open, "delete-file", &error, path.into())
            }
        }
    }

    #[scheme(name = "file-size-in-bytes")]
    pub fn file_size_in_bytes(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => (metadata.len() as i64).into_value(ctx),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-size-in-bytes",
                    &error,
                    path.into(),
                )
            }
        }
    }

    #[scheme(name = "file-regular?")]
    pub fn file_regular(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => Value::new(metadata.is_file()),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(err, IoOperation::Open, "file-regular?", &error, path.into())
            }
        }
    }

    #[scheme(name = "file-directory?")]
    pub fn file_directory(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => Value::new(metadata.is_dir()),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-directory?",
                    &error,
                    path.into(),
                )
            }
        }
    }

    #[scheme(name = "file-symbolic-link?")]
    pub fn file_symbolic_link(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::symlink_metadata(path.to_string()) {
            Ok(metadata) => Value::new(metadata.file_type().is_symlink()),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-symbolic-link?",
                    &error,
                    path.into(),
                )
            }
        }
    }

    #[scheme(name = "file-readable?")]
    pub fn file_readable(path: Gc<'gc, Str<'gc>>) -> bool {
        rustix::fs::access(path.to_string(), rustix::fs::Access::READ_OK).is_ok()
    }

    #[scheme(name = "file-writable?")]
    pub fn file_writable(path: Gc<'gc, Str<'gc>>) -> bool {
        rustix::fs::access(path.to_string(), rustix::fs::Access::WRITE_OK).is_ok()
    }

    #[scheme(name = "file-executable?")]
    pub fn file_executable(path: Gc<'gc, Str<'gc>>) -> bool {
        rustix::fs::access(path.to_string(), rustix::fs::Access::EXEC_OK).is_ok()
    }

    #[scheme(name = "change-file-mode")]
    pub fn change_file_mode(path: Gc<'gc, Str<'gc>>, mode: i32) -> Value<'gc> {
        let Some(mode) = rustix::fs::Mode::from_bits(mode as _) else {
            return call.wrong_argument_violation(
                "change-file-mode",
                "invalid mode bits",
                Some(mode.into()),
                Some(0),
                2,
                &[path.into(), mode.into()],
            );
        };
        match rustix::fs::chmod(path.to_string(), mode) {
            Ok(()) => Value::new(true),
            Err(err) => call.raise_io_error(
                std::io::Error::new(err.kind(), "failed to change mode"),
                IoOperation::Open,
                "change-file-mode",
                &err.to_string(),
                path.into(),
            ),
        }
    }

    #[scheme(name = "rename-file")]
    pub fn rename_file(old_path: Gc<'gc, Str<'gc>>, new_path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::rename(old_path.to_string(), new_path.to_string()) {
            Ok(()) => Value::new(true),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_filesystem_error(
                    "rename-file",
                    &error,
                    err,
                    old_path.into(),
                    new_path.into(),
                )
            }
        }
    }

    #[scheme(name = "canonicalize-path-string")]
    pub fn canonicalize_path(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::canonicalize(path.to_string()) {
            Ok(buf) => {
                let s = buf.to_string_lossy();
                Str::new(ctx, &s, true).into()
            }
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "canonicalize-path",
                    &error,
                    path.into(),
                )
            }
        }
    }

    #[scheme(name = "create-symbolic-link")]
    pub fn create_symbolic_link(
        original: Gc<'gc, Str<'gc>>,
        link: Gc<'gc, Str<'gc>>,
    ) -> Value<'gc> {
        match std::os::unix::fs::symlink(original.to_string(), link.to_string()) {
            Ok(()) => Value::new(true),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "create-symbolic-link",
                    &error,
                    link.into(),
                )
            }
        }
    }

    #[scheme(name = "create-hard-link")]
    pub fn create_hard_link(original: Gc<'gc, Str<'gc>>, link: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::hard_link(original.to_string(), link.to_string()) {
            Ok(()) => Value::new(true),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "create-hard-link",
                    &error,
                    link.into(),
                )
            }
        }
    }

    #[scheme(name = "file-stat-atime")]
    pub fn file_stat_atime(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => match metadata.accessed() {
                Ok(time) => match time.duration_since(std::time::UNIX_EPOCH) {
                    Ok(dur) => (dur.as_secs() as i64).into_value(ctx),
                    Err(_) => (-1i64).into_value(ctx),
                },
                Err(_) => (-1i64).into_value(ctx),
            },
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-stat-atime",
                    &error,
                    path.into(),
                )
            }
        }
    }

    #[scheme(name = "file-stat-mtime")]
    pub fn file_stat_mtime(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => match metadata.modified() {
                Ok(time) => match time.duration_since(std::time::UNIX_EPOCH) {
                    Ok(dur) => (dur.as_secs() as i64).into_value(ctx),
                    Err(_) => (-1i64).into_value(ctx),
                },
                Err(_) => (-1i64).into_value(ctx),
            },
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-stat-mtime",
                    &error,
                    path.into(),
                )
            }
        }
    }

    #[scheme(name = "file-stat-ctime")]
    pub fn file_stat_ctime(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => match metadata.created() {
                Ok(time) => match time.duration_since(std::time::UNIX_EPOCH) {
                    Ok(dur) => (dur.as_secs() as i64).into_value(ctx),
                    Err(_) => (-1i64).into_value(ctx),
                },
                Err(_) => (-1i64).into_value(ctx),
            },
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-stat-ctime",
                    &error,
                    path.into(),
                )
            }
        }
    }

    #[scheme(name = "getcwd")]
    pub fn getcwd() -> Gc<'gc, Str<'gc>> {
        let buf = std::env::current_dir().unwrap_or_default();
        let s = buf.to_string_lossy();
        Str::new(ctx, &s, true)
    }

    #[scheme(name = "file-canonicalize")]
    pub fn file_canonicalize(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::canonicalize(path.to_string()) {
            Ok(buf) => {
                let s = buf.to_string_lossy();
                (Str::new(ctx, &s, true)).into()
            }
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-canonicalize",
                    &error,
                    path.into(),
                )
            }
        }
    }

    #[scheme(name = "file-exists?")]
    pub fn file_exists(path: Gc<'gc, Str<'gc>>) -> bool {
        let metadata = std::path::Path::new(&path.to_string()).exists();
        metadata
    }

    #[scheme(name = "file-name-separator?")]
    pub fn file_name_separator(s: Either<StringRef<'gc>, char>) -> bool {
        let sep = std::path::MAIN_SEPARATOR;
        match s {
            Either::Left(s) => {
                let s_str = s.to_string();

                s_str.len() == 1
                    && s_str
                        .chars()
                        .next()
                        .expect("non-empty string should have first char")
                        == sep
            }
            Either::Right(c) => c == sep,
        }
    }

    #[scheme(name = "system-file-name-convention")]
    pub fn system_filename_conv() -> Gc<'gc, Symbol<'gc>> {
        static_symbols! {
            SYM_WINDOWS = "windows"
            SYM_UNIX = "unix"
        }

        let sym = if cfg!(windows) {
            sym_windows(ctx)
        } else {
            sym_unix(ctx)
        };

        sym
    }

    #[scheme(name = "absolute-path-string?")]
    pub fn is_absolute_path_string(path: Gc<'gc, Str<'gc>>) -> bool {
        let p = std::path::PathBuf::from(path.to_string());
        p.is_absolute()
    }

    #[scheme(name = "io/open")]
    pub fn io_open(path: Gc<'gc, Str<'gc>>, oflag: i32, pmode: i32) -> i32 {
        let path = path.to_string();

        // SAFETY: FFI: path is a valid NUL-terminated string with valid flags
        unsafe {
            let cpath = std::ffi::CString::new(path).expect("path should not contain null bytes");
            libc::open(cpath.as_ptr(), oflag, pmode)
        }
    }

    #[scheme(name = "term/isatty?")]
    pub fn term_isatty(fd: i32) -> bool {
        // SAFETY: FFI: fd is a valid open file descriptor
        unsafe { libc::isatty(fd) == 1 }
    }

    #[scheme(name = "term/open-tty")]
    pub fn term_open_tty(readable: bool, writable: bool) -> Value<'gc> {
        let flags = if readable && writable {
            libc::O_RDWR
        } else if writable {
            libc::O_WRONLY
        } else {
            libc::O_RDONLY
        } | libc::O_CLOEXEC;

        // SAFETY: FFI: path is a valid NUL-terminated string with valid flags
        unsafe {
            let path = CString::new("/dev/tty").expect("static tty path");
            let ret = libc::open(path.as_ptr(), flags);
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "term/open-tty",
                    &msg,
                    Value::new(false),
                );
            }
            (ret).into()
        }
    }

    #[scheme(name = "term/close-fd")]
    pub fn term_close_fd(fd: i32) -> Value<'gc> {
        // SAFETY: FFI: fd is a valid open file descriptor
        unsafe {
            let ret = libc::close(fd);
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Close,
                    "term/close-fd",
                    &msg,
                    fd.into(),
                );
            }
            (ret).into()
        }
    }

    #[scheme(name = "term/pipe")]
    pub fn term_pipe() -> Value<'gc> {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let mut fds = [0i32; 2];
            let ret = libc::pipe(fds.as_mut_ptr());
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "term/pipe",
                    &msg,
                    Value::new(false),
                );
            }

            let list = Value::cons(
                ctx,
                fds[0].into(),
                Value::cons(ctx, fds[1].into(), Value::null()),
            );
            list
        }
    }

    #[scheme(name = "term/get-flags")]
    pub fn term_get_flags(fd: i32) -> Value<'gc> {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let ret = libc::fcntl(fd, libc::F_GETFL);
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Stat,
                    "term/get-flags",
                    &msg,
                    fd.into(),
                );
            }
            (ret).into()
        }
    }

    #[scheme(name = "term/set-flags!")]
    pub fn term_set_flags(fd: i32, flags: i32) -> Value<'gc> {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let ret = libc::fcntl(fd, libc::F_SETFL, flags);
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Stat,
                    "term/set-flags!",
                    &msg,
                    fd.into(),
                );
            }
            (ret).into()
        }
    }

    #[scheme(name = "term/nonblocking!")]
    pub fn term_nonblocking(fd: i32, enabled: bool) -> Value<'gc> {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let flags = libc::fcntl(fd, libc::F_GETFL);
            if flags < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Stat,
                    "term/nonblocking!",
                    &msg,
                    fd.into(),
                );
            }

            let new_flags = if enabled {
                flags | libc::O_NONBLOCK
            } else {
                flags & !libc::O_NONBLOCK
            };
            if libc::fcntl(fd, libc::F_SETFL, new_flags) < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Stat,
                    "term/nonblocking!",
                    &msg,
                    fd.into(),
                );
            }
            (true).into()
        }
    }

    #[scheme(name = "io/read")]
    pub fn io_read(fd: i32, buf: Gc<'gc, ByteVector>, from: usize, nbytes: usize) -> Value<'gc> {
        if from + nbytes > buf.len() {
            return call.wrong_argument_violation(
                "io/read",
                "buffer too small for read",
                None,
                None,
                4,
                &[
                    fd.into(),
                    buf.into(),
                    from.into_value(ctx),
                    nbytes.into_value(ctx),
                ],
            );
        }

        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let ptr = buf.as_slice_mut_unchecked().as_mut_ptr().add(from) as *mut libc::c_void;
            let ret = libc::read(fd, ptr, nbytes);
            (ret).into_value(ctx)
        }
    }

    #[scheme(name = "term/read-fd")]
    pub fn term_read_fd(
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        start: usize,
        count: usize,
    ) -> Value<'gc> {
        if start + count > buf.len() {
            return call.wrong_argument_violation(
                "term/read-fd",
                "buffer too small for read",
                None,
                None,
                4,
                &[
                    fd.into(),
                    buf.into(),
                    start.into_value(ctx),
                    count.into_value(ctx),
                ],
            );
        }

        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let ptr = buf.as_slice_mut_unchecked().as_mut_ptr().add(start) as *mut libc::c_void;
            let ret = libc::read(fd, ptr, count);
            if ret < 0 {
                let errno = errno::errno().0;
                if errno == libc::EAGAIN || errno == libc::EWOULDBLOCK {
                    return (0).into();
                }
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Read,
                    "term/read-fd",
                    &msg,
                    fd.into(),
                );
            }
            (ret).into_value(ctx)
        }
    }

    #[scheme(name = "io/write")]
    pub fn io_write(fd: i32, buf: Gc<'gc, ByteVector>, from: usize, nbytes: usize) -> Value<'gc> {
        if from + nbytes > buf.len() {
            return call.wrong_argument_violation(
                "io/write",
                "buffer too small for write",
                None,
                None,
                4,
                &[
                    fd.into(),
                    buf.into(),
                    from.into_value(ctx),
                    nbytes.into_value(ctx),
                ],
            );
        }

        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let ptr = buf.as_slice().as_ptr().add(from) as *mut libc::c_void;
            let ret = libc::write(fd, ptr, nbytes);
            (ret).into_value(ctx)
        }
    }

    #[scheme(name = "term/write-fd")]
    pub fn term_write_fd(
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        start: usize,
        count: usize,
    ) -> Value<'gc> {
        if start + count > buf.len() {
            return call.wrong_argument_violation(
                "term/write-fd",
                "buffer too small for write",
                None,
                None,
                4,
                &[
                    fd.into(),
                    buf.into(),
                    start.into_value(ctx),
                    count.into_value(ctx),
                ],
            );
        }

        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let ptr = buf.as_slice().as_ptr().add(start) as *const libc::c_void;
            let ret = libc::write(fd, ptr, count);
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Write,
                    "term/write-fd",
                    &msg,
                    fd.into(),
                );
            }
            (ret).into_value(ctx)
        }
    }

    #[scheme(name = "term/sigwinch-version")]
    pub fn term_sigwinch_version() -> u64 {
        TERM_SIGWINCH_VERSION.load(Ordering::Relaxed)
    }

    #[scheme(name = "term/install-sigwinch-handler!")]
    pub fn term_install_sigwinch_handler() -> Value<'gc> {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let mut action: libc::sigaction = std::mem::zeroed();
            action.sa_sigaction = term_sigwinch_handler as *const () as usize;
            action.sa_flags = 0;
            libc::sigemptyset(&mut action.sa_mask);

            if libc::sigaction(libc::SIGWINCH, &action, std::ptr::null_mut()) < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Stat,
                    "term/install-sigwinch-handler!",
                    &msg,
                    Value::new(false),
                );
            }
            (true).into()
        }
    }

    #[scheme(name = "term/enable-raw-mode!")]
    pub fn term_enable_raw_mode() -> Value<'gc> {
        let mut state = TERM_RAW_MODE_STATE.lock().expect("raw mode mutex poisoned");
        if state.is_some() {
            return Value::new(true);
        }

        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let fd = match open_tty_raw_fd() {
                Ok(fd) => fd,
                Err(err) => {
                    let msg = err.to_string();
                    return call.raise_io_error(
                        err,
                        IoOperation::Open,
                        "term/enable-raw-mode!",
                        &msg,
                        Value::new(false),
                    );
                }
            };

            let mut saved: libc::termios = std::mem::zeroed();
            if libc::tcgetattr(fd, &mut saved) < 0 {
                let _ = libc::close(fd);
                return term_io_error(call, IoOperation::Stat, "term/enable-raw-mode!", fd.into());
            }

            let mut raw = saved;
            libc::cfmakeraw(&mut raw);
            if libc::tcsetattr(fd, libc::TCSANOW, &raw) < 0 {
                let _ = libc::close(fd);
                return term_io_error(call, IoOperation::Stat, "term/enable-raw-mode!", fd.into());
            }

            *state = Some((fd, saved));
            Value::new(true)
        }
    }

    #[scheme(name = "term/disable-raw-mode!")]
    pub fn term_disable_raw_mode() -> Value<'gc> {
        let mut state = TERM_RAW_MODE_STATE.lock().expect("raw mode mutex poisoned");
        let Some((fd, saved)) = *state else {
            return Value::new(true);
        };

        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            if libc::tcsetattr(fd, libc::TCSANOW, &saved) < 0 {
                return term_io_error(call, IoOperation::Stat, "term/disable-raw-mode!", fd.into());
            }

            if libc::close(fd) < 0 {
                return term_io_error(
                    call,
                    IoOperation::Close,
                    "term/disable-raw-mode!",
                    fd.into(),
                );
            }
        }

        *state = None;
        Value::new(true)
    }

    #[scheme(name = "term/raw-mode-enabled?")]
    pub fn term_raw_mode_enabled() -> bool {
        TERM_RAW_MODE_STATE
            .lock()
            .expect("raw mode mutex poisoned")
            .is_some()
    }

    #[scheme(name = "term/terminal-size-list")]
    pub fn term_terminal_size_list() -> Value<'gc> {
        // SAFETY: FFI: path is a valid NUL-terminated string with valid flags
        unsafe {
            let tty_fd = {
                let path = CString::new("/dev/tty").expect("static tty path");
                let fd = libc::open(path.as_ptr(), libc::O_RDONLY | libc::O_CLOEXEC);
                if fd < 0 { None } else { Some(fd) }
            };
            let fd = tty_fd.unwrap_or(1);

            let mut size: libc::winsize = std::mem::zeroed();
            let result = libc::ioctl(fd, libc::TIOCGWINSZ, &mut size);
            if let Some(opened_fd) = tty_fd {
                let _ = libc::close(opened_fd);
            }
            if result < 0 {
                return term_io_error(
                    call,
                    IoOperation::Stat,
                    "term/terminal-size-list",
                    fd.into(),
                );
            }

            let mut result = Value::null();
            result = Value::cons(ctx, (size.ws_ypixel as i32).into(), result);
            result = Value::cons(ctx, (size.ws_xpixel as i32).into(), result);
            result = Value::cons(ctx, (size.ws_row as i32).into(), result);
            result = Value::cons(ctx, (size.ws_col as i32).into(), result);
            result
        }
    }

    #[scheme(name = "io/pread")]
    pub fn io_pread(
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        from: usize,
        nbytes: usize,
        offset: i64,
    ) -> Value<'gc> {
        if from + nbytes > buf.len() {
            return call.wrong_argument_violation(
                "io/pread",
                "buffer too small for pread",
                None,
                None,
                5,
                &[
                    fd.into(),
                    buf.into(),
                    from.into_value(ctx),
                    nbytes.into_value(ctx),
                    offset.into_value(ctx),
                ],
            );
        }

        // SAFETY: Mutable access is exclusive and goes through GC write barrier
        unsafe {
            let ptr = buf.as_slice_mut_unchecked().as_mut_ptr().add(from) as *mut libc::c_void;
            let ret = libc::pread(fd, ptr, nbytes, offset);
            (ret).into_value(ctx)
        }
    }

    #[scheme(name = "io/pwrite")]
    pub fn io_pwrite(
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        from: usize,
        nbytes: usize,
        offset: i64,
    ) -> Value<'gc> {
        if from + nbytes > buf.len() {
            return call.wrong_argument_violation(
                "io/pwrite",
                "buffer too small for pwrite",
                None,
                None,
                5,
                &[
                    fd.into(),
                    buf.into(),
                    from.into_value(ctx),
                    nbytes.into_value(ctx),
                    offset.into_value(ctx),
                ],
            );
        }

        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let ptr = buf.as_slice().as_ptr().add(from) as *const libc::c_void;
            let ret = libc::pwrite(fd, ptr, nbytes, offset);
            (ret).into_value(ctx)
        }
    }

    #[scheme(name = "io/lseek")]
    pub fn io_lseek64(fd: i32, offset: i64, whence: i32) -> i64 {
        // SAFETY: FFI: fd is valid, whence is a valid seek origin
        unsafe {
            let ret = libc::lseek(fd, offset, whence);
            ret
        }
    }

    #[scheme(name = "io/mkstemp")]
    pub fn io_mkstemp(template: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        if template.len() > 128 {
            return call.wrong_argument_violation(
                "io/mkstemp",
                "template too long",
                None,
                None,
                1,
                &[template.into()],
            );
        }
        let mut arr = [0u8; 256];
        let templ = format!("{template}_XXXXXX");
        arr[..templ.len()].copy_from_slice(templ.as_bytes());

        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let ret = libc::mkstemp(arr.as_mut_ptr() as *mut _);
            let newname = std::ffi::CStr::from_ptr(arr.as_ptr() as _).to_string_lossy();
            let gname = Str::new(ctx, &newname, true);
            let res = crate::list!(ctx, ret, gname);
            res
        }
    }

    #[scheme(name = "syscall:open")]
    pub fn syscall_open(filename: Gc<'gc, Str<'gc>>, flags: i32, mode: i32) -> i32 {
        let mut newflags = 0;

        if flags & 0x01 != 0 && flags & 0x02 == 0 {
            newflags |= libc::O_RDONLY;
        }

        if flags & 0x02 != 0 && flags & 0x01 == 0 {
            newflags |= libc::O_WRONLY;
        }

        if flags & 0x02 != 0 && flags & 0x01 != 0 {
            newflags = libc::O_RDWR;
        }

        if flags & 0x04 != 0 {
            newflags |= libc::O_APPEND;
        }

        if flags & 0x08 != 0 {
            newflags |= libc::O_CREAT;
        }

        if flags & 0x10 != 0 {
            newflags |= libc::O_TRUNC;
        }

        if filename.is_empty() {
            return -1;
        }

        // SAFETY: FFI: path is a valid NUL-terminated string with valid flags
        unsafe {
            let cpath =
                CString::new(filename.to_string()).expect("filename should not contain null bytes");
            let ret = libc::open(cpath.as_ptr(), newflags | libc::O_NONBLOCK, mode);
            ret
        }
    }

    #[scheme(name = "syscall:close")]
    pub fn syscall_close(fd: i32) -> i32 {
        // SAFETY: FFI: fd is a valid open file descriptor
        unsafe {
            let ret = libc::close(fd);

            ret
        }
    }

    #[scheme(name = "syscall:read")]
    pub fn syscall_read(
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        nbytes: usize,
        start: Option<usize>,
    ) -> Value<'gc> {
        let start = start.unwrap_or(0);
        if start.checked_add(nbytes).is_none_or(|end| end > buf.len()) {
            return call.wrong_argument_violation(
                "syscall:read",
                "buffer too small for read",
                None,
                None,
                3,
                &[fd.into(), buf.into(), nbytes.into_value(ctx)],
            );
        }

        let mut buf = buf;
        loop {
            // SAFETY: Mutable access is exclusive and goes through GC write barrier
            let ret = unsafe {
                let ptr = buf.as_slice_mut_unchecked().as_mut_ptr().add(start) as *mut libc::c_void;
                libc::read(fd, ptr, nbytes)
            };
            if ret == -1 && errno::errno().0 == libc::EINTR {
                continue;
            }
            if ret != -1
                || (errno::errno().0 != libc::EAGAIN && errno::errno().0 != libc::EWOULDBLOCK)
            {
                return ret.into_value(ctx);
            }
            let scope = RootScope::new(ctx);
            let buf_root = scope.root(buf.into());
            ctx.call_in_native(|| blocking_poll_read(fd));
            buf = buf_root.get().downcast();
        }
    }

    #[scheme(name = "syscall:write")]
    pub fn syscall_write(
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        nbytes: usize,
        offset: usize,
    ) -> Value<'gc> {
        if nbytes > buf.len() {
            return call.wrong_argument_violation(
                "syscall:write",
                "buffer too small for write",
                None,
                None,
                3,
                &[fd.into(), buf.into(), nbytes.into_value(ctx)],
            );
        }

        let mut buf = buf;
        loop {
            // SAFETY: Preconditions verified by the surrounding code
            let ret = unsafe {
                let ptr = buf.as_slice().as_ptr().wrapping_add(offset) as *const libc::c_void;
                libc::write(fd, ptr, nbytes)
            };
            if ret == -1 && errno::errno().0 == libc::EINTR {
                continue;
            }
            if ret != -1
                || (errno::errno().0 != libc::EAGAIN && errno::errno().0 != libc::EWOULDBLOCK)
            {
                return ret.into_value(ctx);
            }
            let scope = RootScope::new(ctx);
            let buf_root = scope.root(buf.into());
            ctx.call_in_native(|| blocking_poll_write(fd));
            buf = buf_root.get().downcast();
        }
    }

    #[scheme(name = "syscall:lseek")]
    pub fn syscall_lseek(fd: i32, offset: i64, whence: i32) -> i64 {
        let whence = match whence {
            0 => libc::SEEK_SET,
            1 => libc::SEEK_CUR,
            2 => libc::SEEK_END,
            _ => return -1,
        };
        loop {
            // SAFETY: FFI: fd is valid, whence is a valid seek origin
            let ret = unsafe { libc::lseek(fd, offset, whence) };
            if ret != -1
                || (errno::errno().0 != libc::EAGAIN && errno::errno().0 != libc::EWOULDBLOCK)
            {
                return ret;
            }
            ctx.call_in_native(|| blocking_poll_write(fd));
        }
    }

    #[scheme(name = "syscall:isatty")]
    pub fn syscall_isatty(fd: i32) -> bool {
        // SAFETY: FFI: isatty accepts any integer descriptor
        unsafe { libc::isatty(fd) == 1 }
    }

    #[scheme(name = "syscall:pollinput")]
    pub fn syscall_pollinput(fd: i32) -> i32 {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let mut fds = libc::pollfd {
                fd,
                events: libc::POLLIN,
                revents: 0,
            };
            let ret = libc::poll(&mut fds as *mut libc::pollfd, 1, 0);
            if ret > 0 && (fds.revents & (libc::POLLIN | libc::POLLHUP | libc::POLLERR)) != 0 {
                1
            } else {
                0
            }
        }
    }

    #[scheme(name = "syscall:unlink")]
    pub fn syscall_unlink(filename: Gc<'gc, Str<'gc>>) -> i32 {
        let cpath =
            CString::new(filename.to_string()).expect("filename should not contain null bytes");
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let ret = libc::unlink(cpath.as_ptr());
            ret
        }
    }

    #[scheme(name = "syscall:rename")]
    pub fn syscall_rename(old_filename: Gc<'gc, Str<'gc>>, new_filename: Gc<'gc, Str<'gc>>) -> i32 {
        let cold = CString::new(old_filename.to_string())
            .expect("old filename should not contain null bytes");
        let cnew = CString::new(new_filename.to_string())
            .expect("new filename should not contain null bytes");
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let ret = libc::rename(cold.as_ptr(), cnew.as_ptr());
            ret
        }
    }

    #[scheme(name = "syscall:mtime")]
    pub fn syscall_mtime(filename: Gc<'gc, Str<'gc>>, vbuf: Gc<'gc, Vector<'gc>>) -> i32 {
        let path = filename.to_string();

        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let mut buf = std::mem::zeroed::<libc::stat>();
            let cpath = CString::new(path).expect("path should not contain null bytes");
            let ret = libc::stat(cpath.as_ptr(), &mut buf);
            if ret != 0 {
                return -1;
            }

            let tm = libc::localtime(&buf.st_mtime);
            let wbuf = Gc::write(ctx, vbuf);
            wbuf[0].unlock().set((1900 + (*tm).tm_year).into_value(ctx));
            wbuf[1].unlock().set((1 + (*tm).tm_mon).into_value(ctx));
            wbuf[2].unlock().set((*tm).tm_mday.into_value(ctx));
            wbuf[3].unlock().set((*tm).tm_hour.into_value(ctx));
            wbuf[4].unlock().set((*tm).tm_min.into_value(ctx));
            wbuf[5].unlock().set((*tm).tm_sec.into_value(ctx));
            0
        }
    }

    #[scheme(name = "syscall:access")]
    pub fn syscall_access(filename: Gc<'gc, Str<'gc>>, mode: i32) -> i32 {
        let mut rmode = 0;

        if mode & 0x01 != 0 {
            rmode |= libc::F_OK;
        }

        if mode & 0x02 != 0 {
            rmode |= libc::R_OK;
        }

        if mode & 0x04 != 0 {
            rmode |= libc::W_OK;
        }

        if mode & 0x08 != 0 {
            rmode |= libc::X_OK;
        }

        let cpath =
            CString::new(filename.to_string()).expect("filename should not contain null bytes");
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let ret = libc::access(cpath.as_ptr(), rmode);
            ret
        }
    }

    #[scheme(name = "syscall:recv")]
    pub fn syscall_recv(
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        nbytes: usize,
        flags: i32,
    ) -> Value<'gc> {
        if nbytes > buf.len() {
            return call.wrong_argument_violation(
                "syscall:recv",
                "buffer too small for recv",
                None,
                None,
                3,
                &[fd.into(), buf.into(), nbytes.into_value(ctx)],
            );
        }

        let mut buf = buf;
        loop {
            // SAFETY: Mutable access is exclusive and goes through GC write barrier
            let ret = unsafe {
                let ptr = buf.as_slice_mut_unchecked().as_mut_ptr() as *mut libc::c_void;
                libc::recv(fd, ptr, nbytes, flags)
            };
            if ret != -1
                || (errno::errno().0 != libc::EAGAIN && errno::errno().0 != libc::EWOULDBLOCK)
            {
                return ret.into_value(ctx);
            }
            let scope = RootScope::new(ctx);
            let buf_root = scope.root(buf.into());
            ctx.call_in_native(|| blocking_poll_read(fd));
            buf = buf_root.get().downcast();
        }
    }

    #[scheme(name = "syscall:send")]
    pub fn syscall_send(
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        nbytes: usize,
        flags: i32,
    ) -> Value<'gc> {
        if nbytes > buf.len() {
            return call.wrong_argument_violation(
                "syscall:send",
                "buffer too small for send",
                None,
                None,
                3,
                &[fd.into(), buf.into(), nbytes.into_value(ctx)],
            );
        }

        let mut buf = buf;
        loop {
            // SAFETY: Preconditions verified by the surrounding code
            let ret = unsafe {
                let ptr = buf.as_slice().as_ptr() as *const libc::c_void;
                libc::send(fd, ptr, nbytes, flags)
            };
            if ret != -1
                || (errno::errno().0 != libc::EAGAIN && errno::errno().0 != libc::EWOULDBLOCK)
            {
                return ret.into_value(ctx);
            }
            let scope = RootScope::new(ctx);
            let buf_root = scope.root(buf.into());
            ctx.call_in_native(|| blocking_poll_write(fd));
            buf = buf_root.get().downcast();
        }
    }

    #[scheme(name = "process-environment->alist")]
    pub fn process_environment_to_alist() -> Value<'gc> {
        let mut alist = Value::null();
        for (key, value) in std::env::vars() {
            let key_str = Str::new(ctx, &key, true);
            let value_str = Str::new(ctx, &value, true);
            let pair = Value::cons(ctx, key_str.into(), value_str.into());
            alist = Value::cons(ctx, pair, alist);
        }
        alist = alist.list_reverse(ctx);
        alist
    }

    #[scheme(name = "system")]
    pub fn system(command: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        let ccommand =
            CString::new(command.to_string()).expect("command should not contain null bytes");
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let ret = libc::system(ccommand.as_ptr());
            if ret != -1 {
                if libc::WIFEXITED(ret) {
                    (libc::WEXITSTATUS(ret)).into()
                } else if libc::WIFSIGNALED(ret) {
                    (-libc::WTERMSIG(ret)).into()
                } else {
                    (-1).into()
                }
            } else {
                let errno = ::errno::errno();

                let message = format!("system() failed: {errno}");
                let errno_code = errno.0;

                call.raise_error("system", &message, &[errno_code.into(), command.into()])
            }
        }
    }

    #[scheme(name = "errno/string")]
    pub fn errno_string() -> (i32, Gc<'gc, Str<'gc>>) {
        let errno = ::errno::errno();
        let message = errno.to_string();
        (errno.0, Str::new(ctx, &message, true))
    }

    #[scheme(name = "acquire-lockfile")]
    pub fn acquire_lockfile(path: Gc<'gc, Str<'gc>>, wait: Option<bool>) -> Value<'gc> {
        let wait = wait.unwrap_or(true);
        // SAFETY: FFI: path is a valid NUL-terminated string with valid flags
        let fd = unsafe {
            libc::open(
                CString::new(path.to_string())
                    .expect("path should not contain null bytes")
                    .as_ptr(),
                libc::O_CREAT,
                (libc::S_IRUSR | libc::S_IWUSR) as libc::c_uint,
            )
        };

        if fd < 0 {
            let err = std::io::Error::last_os_error();
            let error = err.to_string();
            return call.raise_io_error(
                err,
                IoOperation::Open,
                "acquire-lockfile",
                &error,
                path.into(),
            );
        }

        // SAFETY: FFI: fd is valid, operation follows POSIX flock semantics
        if unsafe { libc::flock(fd, libc::LOCK_EX | if wait { 0 } else { libc::LOCK_NB }) } != 0 {
            let err = std::io::Error::last_os_error();
            let error = err.to_string();
            return call.raise_io_error(
                err,
                IoOperation::Open,
                "acquire-lockfile",
                &error,
                path.into(),
            );
        }

        (fd).into()
    }

    #[scheme(name = "release-lockfile")]
    pub fn release_lockfile(fd: i32) -> Value<'gc> {
        // SAFETY: FFI: fd is valid, operation follows POSIX flock semantics
        if unsafe { libc::flock(fd, libc::LOCK_UN) } != 0 {
            let err = std::io::Error::last_os_error();
            let error = err.to_string();
            return call.raise_io_error(
                err,
                IoOperation::Close,
                "release-lockfile",
                &error,
                Value::new(false),
            );
        }

        // SAFETY: FFI: fd is a valid open file descriptor
        if unsafe { libc::close(fd) } != 0 {
            let err = std::io::Error::last_os_error();
            let error = err.to_string();
            return call.raise_io_error(
                err,
                IoOperation::Close,
                "release-lockfile",
                &error,
                Value::new(false),
            );
        }

        (true).into()
    }

    #[scheme(name = "%process-spawn")]
    pub fn process_spawn(string: StringRef<'gc>, pipe_stderr: bool) -> Value<'gc> {
        let child = match std::process::Command::new("sh")
            .arg("-c")
            .arg(string.to_string())
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
        {
            Ok(child) => child,
            Err(err) => {
                let error = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "%process-spawn",
                    &error,
                    string.into(),
                );
            }
        };

        let ifd = child
            .stdin
            .as_ref()
            .expect("child process stdin should be piped")
            .as_raw_fd();
        let ofd = child
            .stdout
            .as_ref()
            .expect("child process stdout should be piped")
            .as_raw_fd();
        let efd = child
            .stderr
            .as_ref()
            .expect("child process stderr should be piped")
            .as_raw_fd();
        let pid = child.id() as i32;
        std::mem::forget(child);
        if pipe_stderr {
            crate::list!(ctx, ifd, efd, ofd, pid)
        } else {
            crate::list!(ctx, ifd, ofd, pid)
        }
    }

    #[scheme(name = "%process-wait")]
    pub fn process_wait(pid: i32) -> Value<'gc> {
        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            let mut status: i32 = 0;
            let ret = libc::waitpid(pid, &mut status as *mut i32, 0);
            if ret == -1 {
                let err = std::io::Error::last_os_error();
                let error = err.to_string();
                return call.raise_io_error(
                    err,
                    IoOperation::Read,
                    "%process-wait",
                    &error,
                    pid.into(),
                );
            }

            if libc::WIFEXITED(status) {
                (libc::WEXITSTATUS(status)).into()
            } else if libc::WIFSIGNALED(status) {
                (-libc::WTERMSIG(status)).into()
            } else {
                (-1).into()
            }
        }
    }

    /* polling primitives */
    #[scheme(name = "make-poller")]
    /// Create a new poller object instance. This can be used
    /// to perform non-blocking I/O operations.
    pub fn make_poller() -> Value<'gc> {
        let inner = match polling::Poller::new() {
            Ok(poller) => poller,
            Err(err) => {
                return call.raise_io_error(
                    err,
                    IoOperation::Open,
                    "make-poller",
                    "failed to create poller",
                    Value::new(false),
                );
            }
        };
        let poller = ctx.allocate_with_header_word(
            Poller { inner },
            poller_header_word(),
            crate::heap::mmtk::AllocationSemantics::Los,
        );

        (poller).into()
    }

    /// Add a file descriptor to the poller with the specified flags.
    ///
    /// Arguments:
    /// - `key`: A unique identifier for the file descriptor.
    /// - `fd`: The file descriptor to monitor.
    /// - `flags`: The events to monitor (e.g., readable, writable).
    #[scheme(name = "poller-add!")]
    pub fn poller_add(poller: Gc<'gc, Poller>, key: usize, fd: i32, flags: i32) -> Value<'gc> {
        // SAFETY: The raw fd is a valid open file descriptor
        let fd = unsafe { std::os::fd::BorrowedFd::borrow_raw(fd) };
        let readable = (flags & EREADABLE) != 0;
        let writable = (flags & EWRITABLE) != 0;
        let mut event = polling::Event::new(key, readable, writable);
        if (flags & EPRIORITY) != 0 {
            event.set_priority(true);
        }
        if (flags & EHUP) != 0 {
            event.set_interrupt(true);
        }

        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            match poller.inner.add(&fd, event) {
                Ok(()) => Value::new(true),
                Err(err) => {
                    let error = err.to_string();
                    call.raise_io_error(
                        err,
                        IoOperation::Write,
                        "poller-add!",
                        &error,
                        Value::new(false),
                    )
                }
            }
        }
    }

    #[scheme(name = "poller-add/mode!")]
    pub fn poller_add_with_mode(
        poller: Gc<'gc, Poller>,
        key: usize,
        fd: i32,
        flags: i32,
        mode: i32,
    ) -> Value<'gc> {
        // SAFETY: The raw fd is a valid open file descriptor
        let fd = unsafe { std::os::fd::BorrowedFd::borrow_raw(fd) };
        let mode = match mode {
            EONESHOT => polling::PollMode::Oneshot,
            ELEVEL => polling::PollMode::Level,
            EEDGE => polling::PollMode::Edge,
            EEDGEONESHOT => polling::PollMode::EdgeOneshot,
            _ => {
                return call.wrong_argument_violation(
                    "poller-add/mode!",
                    "invalid mode",
                    Some(mode.into()),
                    None,
                    5,
                    &[
                        poller.into(),
                        key.into_value(ctx),
                        0i32.into(),
                        flags.into(),
                        mode.into(),
                    ],
                );
            }
        };

        let readable = (flags & EREADABLE) != 0;
        let writable = (flags & EWRITABLE) != 0;
        let mut event = polling::Event::new(key, readable, writable);

        if (flags & EPRIORITY) != 0 {
            event.set_priority(true);
        }

        if (flags & EHUP) != 0 {
            event.set_interrupt(true);
        }

        // SAFETY: Preconditions verified by the surrounding code
        unsafe {
            match poller.inner.add_with_mode(&fd, event, mode) {
                Ok(()) => Value::new(true),
                Err(err) => {
                    let error = err.to_string();
                    call.raise_io_error(
                        err,
                        IoOperation::Write,
                        "poller-add/mode!",
                        &error,
                        Value::new(false),
                    )
                }
            }
        }
    }

    #[scheme(name = "poller-delete!")]
    pub fn poller_delete(poller: Gc<'gc, Poller>, fd: i32) -> Value<'gc> {
        // SAFETY: The raw fd is a valid open file descriptor
        let fd = unsafe { std::os::fd::BorrowedFd::borrow_raw(fd) };
        {
            match poller.inner.delete(fd) {
                Ok(()) => Value::new(true),
                Err(err) => {
                    let error = err.to_string();
                    call.raise_io_error(
                        err,
                        IoOperation::Write,
                        "poller-delete!",
                        &error,
                        Value::new(false),
                    )
                }
            }
        }
    }

    #[scheme(name = "poller-modify!")]
    pub fn poller_modify(poller: Gc<'gc, Poller>, fd: i32, flags: i32) -> Value<'gc> {
        // SAFETY: The raw fd is a valid open file descriptor
        let fd = unsafe { std::os::fd::BorrowedFd::borrow_raw(fd) };
        let readable = (flags & EREADABLE) != 0;
        let writable = (flags & EWRITABLE) != 0;
        let mut event = polling::Event::new(0, readable, writable);

        if (flags & EPRIORITY) != 0 {
            event.set_priority(true);
        }

        if (flags & EHUP) != 0 {
            event.set_interrupt(true);
        }

        {
            match poller.inner.modify(fd, event) {
                Ok(()) => Value::new(true),
                Err(err) => {
                    let error = err.to_string();
                    call.raise_io_error(
                        err,
                        IoOperation::Write,
                        "poller-modify!",
                        &error,
                        Value::new(false),
                    )
                }
            }
        }
    }

    #[scheme(name = "poller-modify/mode!")]
    pub fn poller_modify_with_mode(
        poller: Gc<'gc, Poller>,
        key: usize,
        fd: i32,
        flags: i32,
        mode: i32,
    ) -> Value<'gc> {
        // SAFETY: The raw fd is a valid open file descriptor
        let fd = unsafe { std::os::fd::BorrowedFd::borrow_raw(fd) };
        let mode = match mode {
            EONESHOT => polling::PollMode::Oneshot,
            ELEVEL => polling::PollMode::Level,
            EEDGE => polling::PollMode::Edge,
            EEDGEONESHOT => polling::PollMode::EdgeOneshot,
            _ => {
                return call.wrong_argument_violation(
                    "poller-modify/mode!",
                    "invalid mode",
                    Some(mode.into()),
                    None,
                    5,
                    &[poller.into(), 0i32.into(), flags.into(), mode.into()],
                );
            }
        };

        let readable = (flags & EREADABLE) != 0;
        let writable = (flags & EWRITABLE) != 0;

        let mut event = polling::Event::new(key, readable, writable);

        if (flags & EPRIORITY) != 0 {
            event.set_priority(true);
        }

        if (flags & EHUP) != 0 {
            event.set_interrupt(true);
        }

        {
            match poller.inner.modify_with_mode(fd, event, mode) {
                Ok(()) => Value::new(true),
                Err(err) => {
                    let error = err.to_string();
                    call.raise_io_error(
                        err,
                        IoOperation::Write,
                        "poller-modify/mode!",
                        &error,
                        Value::new(false),
                    )
                }
            }
        }
    }

    #[scheme(name = "poller-wait")]
    /// Waits for at least one I/O event and returns a list of events.
    pub fn poller_wait(poller: Gc<'gc, Poller>, timeout: Option<u64>) -> Value<'gc> {
        let timeout = timeout.map(std::time::Duration::from_micros);
        // Poller is NonMoving, root for liveness across wait.
        let scope = RootScope::new(ctx);
        let poller_root = scope.root(poller.into());
        let poller_ptr = Gc::as_ptr(poller);
        let wait_outcome = ctx.call_in_native(|| {
            // SAFETY: Poller is NonMoving and rooted for the wait.
            let poller = unsafe { &*poller_ptr };
            let mut events = polling::Events::new();
            let res = poller.inner.wait(&mut events, timeout);
            let mut collected = Vec::new();
            if res.is_ok() {
                for event in events.iter() {
                    collected.push((event.key, event_to_flags(event)));
                }
            }
            (
                res.is_ok(),
                res.err().and_then(|e| e.raw_os_error()),
                collected,
            )
        });
        let _ = poller_root.get();
        drop(scope);

        let (ok, os_error, events) = wait_outcome;
        if ok {
            let mut ls = Value::null();
            for (key, flags) in events {
                ls = Value::cons(
                    ctx,
                    Value::cons(ctx, key.into_value(ctx), flags.into_value(ctx)),
                    ls,
                );
            }
            ls
        } else {
            let err = os_error.unwrap_or(i32::MAX);
            call.raise_error(
                "poller-wait",
                "failed to wait for events",
                &[err.into_value(ctx)],
            )
        }
    }

    #[scheme(name = "poller-notify")]
    /// Wakes up current or future `poller-wait` invocation.
    pub fn poller_notify(poller: Gc<'gc, Poller>) -> Value<'gc> {
        match poller.inner.notify() {
            Ok(()) => Value::new(true),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Write,
                    "poller-notify",
                    &error,
                    Value::new(false),
                )
            }
        }
    }

    /// Serialize DATUM with the runtime FASL writer (compression off) to the
    /// file at PATH.
    #[scheme(name = "fasl-write")]
    pub fn fasl_write(datum: Value<'gc>, path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::File::create(path.to_string()) {
            Ok(file) => {
                let writer = Writer::new(ctx, std::io::BufWriter::new(file));
                match writer.write(datum) {
                    Ok(()) => Value::new(true),
                    Err(err) => {
                        let error = err.to_string();
                        call.raise_io_error(
                            err,
                            IoOperation::Write,
                            "fasl-write",
                            &error,
                            path.into(),
                        )
                    }
                }
            }
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(err, IoOperation::Open, "fasl-write", &error, path.into())
            }
        }
    }

    /// Deserialize one value from the FASL file at PATH.
    #[scheme(name = "fasl-read")]
    pub fn fasl_read(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::fs::File::open(path.to_string()) {
            Ok(file) => {
                let reader = Reader::new(ctx, std::io::BufReader::new(file));
                match reader.read() {
                    Ok(value) => value,
                    Err(err) => {
                        let error = err.to_string();
                        call.raise_io_error(
                            err,
                            IoOperation::Read,
                            "fasl-read",
                            &error,
                            path.into(),
                        )
                    }
                }
            }
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(err, IoOperation::Open, "fasl-read", &error, path.into())
            }
        }
    }

    /// Serialize DATUM to a bytevector.
    #[scheme(name = "fasl-write->bytevector")]
    pub fn fasl_write_to_bytevector(datum: Value<'gc>) -> Value<'gc> {
        let mut bytes: Vec<u8> = Vec::new();
        let writer = Writer::new(ctx, &mut bytes);
        match writer.write(datum) {
            Ok(()) => {
                let bv = ByteVector::new::<false>(ctx, bytes.len(), true);
                bv.copy_from(bytes.as_slice());
                bv.into()
            }
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Write,
                    "fasl-write->bytevector",
                    &error,
                    Value::new(false),
                )
            }
        }
    }

    /// Deserialize one value from the FASL bytevector BV.
    #[scheme(name = "fasl-read-bytevector")]
    pub fn fasl_read_bytevector(bv: Gc<'gc, ByteVector>) -> Value<'gc> {
        let bytes = bv.as_slice();
        let reader = Reader::new(ctx, std::io::Cursor::new(bytes));
        match reader.read() {
            Ok(value) => value,
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Read,
                    "fasl-read-bytevector",
                    &error,
                    bv.into(),
                )
            }
        }
    }
}

pub fn init_io<'gc>(ctx: MutationContext<'gc>) {
    io_ops::register(ctx);

    let module = ctx.module("capy").expect("'capy' module should exist");

    macro_rules! export_term_primitive {
        ($name: literal, $make_static: ident) => {{
            let closure = io_ops::$make_static(ctx);
            module.define_rs(ctx, $name, closure);
            module.export_one(ctx, ctx.intern($name));
        }};
    }

    export_term_primitive!("term/isatty?", make_static_closure_term_isatty);
    export_term_primitive!("term/open-tty", make_static_closure_term_open_tty);
    export_term_primitive!("term/close-fd", make_static_closure_term_close_fd);
    export_term_primitive!("term/pipe", make_static_closure_term_pipe);
    export_term_primitive!("term/get-flags", make_static_closure_term_get_flags);
    export_term_primitive!("term/set-flags!", make_static_closure_term_set_flags);
    export_term_primitive!("term/nonblocking!", make_static_closure_term_nonblocking);
    export_term_primitive!("term/read-fd", make_static_closure_term_read_fd);
    export_term_primitive!("term/write-fd", make_static_closure_term_write_fd);
    export_term_primitive!(
        "term/sigwinch-version",
        make_static_closure_term_sigwinch_version
    );
    export_term_primitive!(
        "term/install-sigwinch-handler!",
        make_static_closure_term_install_sigwinch_handler
    );
    export_term_primitive!(
        "term/enable-raw-mode!",
        make_static_closure_term_enable_raw_mode
    );
    export_term_primitive!(
        "term/disable-raw-mode!",
        make_static_closure_term_disable_raw_mode
    );
    export_term_primitive!(
        "term/raw-mode-enabled?",
        make_static_closure_term_raw_mode_enabled
    );
    export_term_primitive!(
        "term/terminal-size-list",
        make_static_closure_term_terminal_size_list
    );

    let host_features_tab = HashTable::new(ctx, HashTableType::Eq, 64, 0.80);
    module.define(ctx, ctx.intern("*host-features*"), host_features_tab.into());

    macro_rules! cconst {
        ($name: ident) => {
            let name = stringify!($name);
            let sym = ctx.intern(name);
            host_features_tab.put(ctx, sym, Value::new(libc::$name));
        };
    }

    macro_rules! cvalue {
        ($name: ident, $val: expr) => {
            let name = stringify!($name);
            let sym = ctx.intern(name);
            host_features_tab.put(ctx, sym, Value::new($val));
        };
    }

    cvalue!(ADDRESS_BITS, usize::BITS as i32);

    cconst!(AF_UNSPEC);
    cconst!(AF_INET);
    cconst!(AF_INET6);
    cconst!(AF_UNIX);
    cconst!(SOCK_STREAM);
    cconst!(SOCK_DGRAM);
    cconst!(SOCK_RAW);
    cconst!(AI_PASSIVE);
    cconst!(AI_CANONNAME);
    cconst!(AI_NUMERICHOST);
    cconst!(AI_NUMERICSERV);
    cconst!(AI_V4MAPPED);
    cconst!(AI_ALL);
    cconst!(AI_ADDRCONFIG);
    cconst!(IPPROTO_IP);
    cconst!(IPPROTO_ICMP);
    cconst!(IPPROTO_TCP);
    cconst!(IPPROTO_UDP);
    cconst!(IPPROTO_RAW);
    cconst!(SHUT_RD);
    cconst!(SHUT_WR);
    cconst!(SHUT_RDWR);
    cconst!(MSG_OOB);
    cconst!(MSG_PEEK);
    cconst!(MSG_DONTROUTE);
    cconst!(MSG_CTRUNC);

    cconst!(MSG_TRUNC);
    cconst!(MSG_DONTWAIT);
    cconst!(MSG_EOR);
    cconst!(MSG_WAITALL);
    #[cfg(target_os = "linux")]
    cconst!(MSG_FIN);
    #[cfg(target_os = "linux")]
    cconst!(MSG_SYN);
    #[cfg(target_os = "linux")]
    cconst!(MSG_CONFIRM);
    #[cfg(target_os = "linux")]
    cconst!(MSG_RST);
    #[cfg(target_os = "linux")]
    cconst!(MSG_ERRQUEUE);
    cconst!(MSG_NOSIGNAL);
    #[cfg(target_os = "linux")]
    cconst!(MSG_MORE);
}

#[repr(C)]
pub struct Poller {
    inner: polling::Poller,
}

fn poller_header_word() -> u64 {
    class_header_word(ClassId::new(builtin_class_ids::POLLER).expect("builtin class id is nonzero"))
}

// SAFETY: Class IDs in `CLASS_IDS` match the allocation header for `Poller`
unsafe impl ClassTagged for Poller {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::POLLER];
    const TYPE_NAME: &'static str = "poller";
}

// SAFETY: GC trace for `Poller`, all reachable heap fields are visited
unsafe impl Trace for Poller {
    // SAFETY: All GC-reachable fields are traced via `visitor`
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        let _ = visitor;
    }

    // SAFETY: Weak refs are processed through the given weak_processor
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let _ = weak_processor;
    }
}

pub const EREADABLE: i32 = 0x01;
pub const EWRITABLE: i32 = 0x02;
pub const EHUP: i32 = 0x04;
pub const EPRIORITY: i32 = 0x08;
pub const EERROR: i32 = 0x10;

pub const EONESHOT: i32 = 0x01;
pub const ELEVEL: i32 = 0x02;
pub const EEDGE: i32 = 0x04;
pub const EEDGEONESHOT: i32 = 0x08;

fn blocking_poll_read(fd: i32) {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let mut readfs = [rustix::event::FdSetElement::default(); 1];
        rustix::event::fd_set_insert(&mut readfs, fd);
        let _ = rustix::event::select(readfs.len() as _, Some(&mut readfs), None, None, None);
    }
}

fn blocking_poll_write(fd: i32) {
    // SAFETY: Preconditions verified by the surrounding code
    unsafe {
        let mut writefs = [rustix::event::FdSetElement::default(); 1];
        rustix::event::fd_set_insert(&mut writefs, fd);
        let _ = rustix::event::select(writefs.len() as _, None, Some(&mut writefs), None, None);
    }
}

fn event_to_flags(ev: polling::Event) -> i32 {
    let mut flags = 0;
    if ev.readable {
        flags |= EREADABLE;
    }
    if ev.writable {
        flags |= EWRITABLE;
    }
    if ev.is_interrupt() {
        flags |= EHUP;
    }
    if ev.is_priority() {
        flags |= EPRIORITY;
    }

    if let Some(true) = ev.is_err() {
        flags |= EERROR;
    }
    flags
}
