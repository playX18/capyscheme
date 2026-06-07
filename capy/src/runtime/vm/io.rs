use crate::prelude::*;
use crate::rsgc::object::{ClassId, builtin_class_ids, class_header_word};
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

fn term_io_error<'gc, R: TryIntoValues<'gc>>(
    nctx: NativeCallContext<'_, 'gc, R>,
    operation: IoOperation,
    who: &'static str,
    irritant: Value<'gc>,
) -> NativeCallReturn<'gc> {
    let err = std::io::Error::last_os_error();
    let msg = err.to_string();
    nctx.raise_io_error(err, operation, who, &msg, irritant)
}

unsafe fn open_tty_raw_fd() -> Result<i32, std::io::Error> {
    let path = CString::new("/dev/tty").expect("static tty path");
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
        nctx.return_(true)
    }

    #[scheme(name = "home-directory")]
    pub fn home_directory() -> Gc<'gc, Str<'gc>> {
        match std::env::home_dir() {
            Some(buf) => {
                let s = buf.to_string_lossy();
                let ctx = nctx.ctx;
                nctx.return_(Str::new(*ctx, &s, true))
            }
            None => nctx.raise_io_error(
                std::io::Error::new(std::io::ErrorKind::NotFound, "home directory not found"),
                IoOperation::Open,
                "home-directory",
                "home directory not found",
                Value::new(false),
            ),
        }
    }

    #[scheme(name = "gethostname")]
    pub fn gethostname() -> Gc<'gc, Str<'gc>> {
        match hostname::get() {
            Ok(name_osstr) => {
                let name_str = name_osstr.to_string_lossy();
                let ctx = nctx.ctx;
                nctx.return_(Str::new(*ctx, &name_str, true))
            }
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
        let ctx = nctx.ctx;
        nctx.return_(Str::new(*ctx, &s, true))
    }

    #[scheme(name = "getenv")]
    pub fn getenv(var: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::env::var(var.to_string()) {
            Ok(s) => {
                let ctx = nctx.ctx;
                nctx.return_(Str::new(*ctx, &s, true).into())
            }
            Err(_) => nctx.return_(Value::new(false)),
        }
    }

    #[scheme(name = "directory-list")]
    pub fn directory_list(path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        let p = std::path::PathBuf::from(path.to_string());
        let ctx = nctx.ctx;

        match std::fs::read_dir(p) {
            Ok(entries) => {
                let mut ls = Value::null();
                for entry in entries.flatten() {
                    let name = entry.file_name().to_string_lossy().into_owned();
                    ls = Value::cons(ctx, Str::new(*ctx, &name, true).into(), ls);
                }
                ls = ls.list_reverse(ctx);
                nctx.return_(ls)
            }
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
    pub fn current_directory(path: Option<Gc<'gc, Str<'gc>>>) -> Gc<'gc, Str<'gc>> {
        match path {
            Some(path) => match std::env::set_current_dir(path.to_string()) {
                Ok(()) => {
                    let ctx = nctx.ctx;
                    nctx.return_(Str::new(*ctx, path.to_string(), true))
                }
                Err(err) => {
                    let error = err.to_string();
                    nctx.raise_io_error(
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
                    let ctx = nctx.ctx;
                    nctx.return_(Str::new(*ctx, &s, true))
                }
                Err(err) => {
                    let error = err.to_string();
                    nctx.raise_io_error(
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
    pub fn create_directory(path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::create_dir(path.to_string()) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
    pub fn create_directory_all(path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::create_dir_all(path.to_string()) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
    pub fn delete_file(path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::remove_file(path.to_string()) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(err, IoOperation::Open, "delete-file", &error, path.into())
            }
        }
    }

    #[scheme(name = "file-size-in-bytes")]
    pub fn file_size_in_bytes(path: Gc<'gc, Str<'gc>>) -> i64 {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => nctx.return_(metadata.len() as i64),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
    pub fn file_regular(path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => nctx.return_(metadata.is_file()),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(err, IoOperation::Open, "file-regular?", &error, path.into())
            }
        }
    }

    #[scheme(name = "file-directory?")]
    pub fn file_directory(path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => nctx.return_(metadata.is_dir()),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
    pub fn file_symbolic_link(path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::symlink_metadata(path.to_string()) {
            Ok(metadata) => nctx.return_(metadata.file_type().is_symlink()),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
        nctx.return_(rustix::fs::access(path.to_string(), rustix::fs::Access::READ_OK).is_ok())
    }

    #[scheme(name = "file-writable?")]
    pub fn file_writable(path: Gc<'gc, Str<'gc>>) -> bool {
        nctx.return_(rustix::fs::access(path.to_string(), rustix::fs::Access::WRITE_OK).is_ok())
    }

    #[scheme(name = "file-executable?")]
    pub fn file_executable(path: Gc<'gc, Str<'gc>>) -> bool {
        nctx.return_(rustix::fs::access(path.to_string(), rustix::fs::Access::EXEC_OK).is_ok())
    }

    #[scheme(name = "change-file-mode")]
    pub fn change_file_mode(path: Gc<'gc, Str<'gc>>, mode: i32) -> bool {
        let Some(mode) = rustix::fs::Mode::from_bits(mode as _) else {
            return nctx.wrong_argument_violation(
                "change-file-mode",
                "invalid mode bits",
                Some(mode.into()),
                Some(0),
                2,
                &[path.into(), mode.into()],
            );
        };
        match rustix::fs::chmod(path.to_string(), mode) {
            Ok(()) => nctx.return_(true),
            Err(err) => nctx.raise_io_error(
                std::io::Error::new(err.kind(), "failed to change mode"),
                IoOperation::Open,
                "change-file-mode",
                &err.to_string(),
                path.into(),
            ),
        }
    }

    #[scheme(name = "rename-file")]
    pub fn rename_file(old_path: Gc<'gc, Str<'gc>>, new_path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::rename(old_path.to_string(), new_path.to_string()) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_filesystem_error(
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
    pub fn canonicalize_path(path: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        match std::fs::canonicalize(path.to_string()) {
            Ok(buf) => {
                let s = buf.to_string_lossy();
                let ctx = nctx.ctx;
                nctx.return_(Str::new(*ctx, &s, true))
            }
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
    pub fn create_symbolic_link(original: Gc<'gc, Str<'gc>>, link: Gc<'gc, Str<'gc>>) -> bool {
        match std::os::unix::fs::symlink(original.to_string(), link.to_string()) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
    pub fn create_hard_link(original: Gc<'gc, Str<'gc>>, link: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::hard_link(original.to_string(), link.to_string()) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
    pub fn file_stat_atime(path: Gc<'gc, Str<'gc>>) -> i64 {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => match metadata.accessed() {
                Ok(time) => match time.duration_since(std::time::UNIX_EPOCH) {
                    Ok(dur) => nctx.return_(dur.as_secs() as i64),
                    Err(_) => nctx.return_(-1),
                },
                Err(_) => nctx.return_(-1),
            },
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
    pub fn file_stat_mtime(path: Gc<'gc, Str<'gc>>) -> i64 {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => match metadata.modified() {
                Ok(time) => match time.duration_since(std::time::UNIX_EPOCH) {
                    Ok(dur) => nctx.return_(dur.as_secs() as i64),
                    Err(_) => nctx.return_(-1),
                },
                Err(_) => nctx.return_(-1),
            },
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
    pub fn file_stat_ctime(path: Gc<'gc, Str<'gc>>) -> i64 {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => match metadata.created() {
                Ok(time) => match time.duration_since(std::time::UNIX_EPOCH) {
                    Ok(dur) => nctx.return_(dur.as_secs() as i64),
                    Err(_) => nctx.return_(-1),
                },
                Err(_) => nctx.return_(-1),
            },
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
        let ctx = nctx.ctx;
        nctx.return_(Str::new(*ctx, &s, true))
    }

    #[scheme(name = "file-canonicalize")]
    pub fn file_canonicalize(path: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        match std::fs::canonicalize(path.to_string()) {
            Ok(buf) => {
                let s = buf.to_string_lossy();
                let ctx = nctx.ctx;
                nctx.return_(Str::new(*ctx, &s, true))
            }
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
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
        nctx.return_(metadata)
    }

    #[scheme(name = "file-name-separator?")]
    pub fn file_name_separator(s: Either<StringRef<'gc>, char>) -> bool {
        let sep = std::path::MAIN_SEPARATOR;
        match s {
            Either::Left(s) => {
                let s_str = s.to_string();
                nctx.return_(
                    s_str.len() == 1
                        && s_str
                            .chars()
                            .next()
                            .expect("non-empty string should have first char")
                            == sep,
                )
            }
            Either::Right(c) => nctx.return_(c == sep),
        }
    }

    #[scheme(name = "system-file-name-convention")]
    pub fn system_filename_conv() -> Gc<'gc, Symbol<'gc>> {
        static_symbols! {
            SYM_WINDOWS = "windows"
            SYM_UNIX = "unix"
        }

        let sym = if cfg!(windows) {
            sym_windows(nctx.ctx)
        } else {
            sym_unix(nctx.ctx)
        };

        nctx.return_(sym)
    }

    #[scheme(name = "absolute-path-string?")]
    pub fn is_absolute_path_string(path: Gc<'gc, Str<'gc>>) -> bool {
        let p = std::path::PathBuf::from(path.to_string());
        nctx.return_(p.is_absolute())
    }

    #[scheme(name = "io/open")]
    pub fn io_open(path: Gc<'gc, Str<'gc>>, oflag: i32, pmode: i32) -> i32 {
        let path = path.to_string();

        unsafe {
            let cpath = std::ffi::CString::new(path).expect("path should not contain null bytes");
            nctx.return_(libc::open(cpath.as_ptr(), oflag, pmode))
        }
    }

    #[scheme(name = "term/isatty?")]
    pub fn term_isatty(fd: i32) -> bool {
        unsafe { nctx.return_(libc::isatty(fd) == 1) }
    }

    #[scheme(name = "term/open-tty")]
    pub fn term_open_tty(readable: bool, writable: bool) -> i32 {
        let flags = if readable && writable {
            libc::O_RDWR
        } else if writable {
            libc::O_WRONLY
        } else {
            libc::O_RDONLY
        } | libc::O_CLOEXEC;

        unsafe {
            let path = CString::new("/dev/tty").expect("static tty path");
            let ret = libc::open(path.as_ptr(), flags);
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "term/open-tty",
                    &msg,
                    Value::new(false),
                );
            }
            nctx.return_(ret)
        }
    }

    #[scheme(name = "term/close-fd")]
    pub fn term_close_fd(fd: i32) -> i32 {
        unsafe {
            let ret = libc::close(fd);
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return nctx.raise_io_error(
                    err,
                    IoOperation::Close,
                    "term/close-fd",
                    &msg,
                    fd.into(),
                );
            }
            nctx.return_(ret)
        }
    }

    #[scheme(name = "term/pipe")]
    pub fn term_pipe() -> Value<'gc> {
        unsafe {
            let mut fds = [0i32; 2];
            let ret = libc::pipe(fds.as_mut_ptr());
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "term/pipe",
                    &msg,
                    Value::new(false),
                );
            }

            let ctx = nctx.ctx;
            let list = Value::cons(
                ctx,
                fds[0].into(),
                Value::cons(ctx, fds[1].into(), Value::null()),
            );
            nctx.return_(list)
        }
    }

    #[scheme(name = "term/get-flags")]
    pub fn term_get_flags(fd: i32) -> i32 {
        unsafe {
            let ret = libc::fcntl(fd, libc::F_GETFL);
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return nctx.raise_io_error(
                    err,
                    IoOperation::Stat,
                    "term/get-flags",
                    &msg,
                    fd.into(),
                );
            }
            nctx.return_(ret)
        }
    }

    #[scheme(name = "term/set-flags!")]
    pub fn term_set_flags(fd: i32, flags: i32) -> i32 {
        unsafe {
            let ret = libc::fcntl(fd, libc::F_SETFL, flags);
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return nctx.raise_io_error(
                    err,
                    IoOperation::Stat,
                    "term/set-flags!",
                    &msg,
                    fd.into(),
                );
            }
            nctx.return_(ret)
        }
    }

    #[scheme(name = "term/nonblocking!")]
    pub fn term_nonblocking(fd: i32, enabled: bool) -> bool {
        unsafe {
            let flags = libc::fcntl(fd, libc::F_GETFL);
            if flags < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return nctx.raise_io_error(
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
                return nctx.raise_io_error(
                    err,
                    IoOperation::Stat,
                    "term/nonblocking!",
                    &msg,
                    fd.into(),
                );
            }
            nctx.return_(true)
        }
    }

    #[scheme(name = "io/read")]
    pub fn io_read(fd: i32, buf: Gc<'gc, ByteVector>, from: usize, nbytes: usize) -> isize {
        if from + nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
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

        unsafe {
            let ptr = buf.as_slice_mut_unchecked().as_mut_ptr().add(from) as *mut libc::c_void;
            let ret = libc::read(fd, ptr, nbytes);
            nctx.return_(ret)
        }
    }

    #[scheme(name = "term/read-fd")]
    pub fn term_read_fd(fd: i32, buf: Gc<'gc, ByteVector>, start: usize, count: usize) -> isize {
        if start + count > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
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

        unsafe {
            let ptr = buf.as_slice_mut_unchecked().as_mut_ptr().add(start) as *mut libc::c_void;
            let ret = libc::read(fd, ptr, count);
            if ret < 0 {
                let errno = errno::errno().0;
                if errno == libc::EAGAIN || errno == libc::EWOULDBLOCK {
                    return nctx.return_(0);
                }
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return nctx.raise_io_error(
                    err,
                    IoOperation::Read,
                    "term/read-fd",
                    &msg,
                    fd.into(),
                );
            }
            nctx.return_(ret)
        }
    }

    #[scheme(name = "io/write")]
    pub fn io_write(fd: i32, buf: Gc<'gc, ByteVector>, from: usize, nbytes: usize) -> isize {
        if from + nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
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

        unsafe {
            let ptr = buf.as_slice().as_ptr().add(from) as *mut libc::c_void;
            let ret = libc::write(fd, ptr, nbytes);
            nctx.return_(ret)
        }
    }

    #[scheme(name = "term/write-fd")]
    pub fn term_write_fd(fd: i32, buf: Gc<'gc, ByteVector>, start: usize, count: usize) -> isize {
        if start + count > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
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

        unsafe {
            let ptr = buf.as_slice().as_ptr().add(start) as *const libc::c_void;
            let ret = libc::write(fd, ptr, count);
            if ret < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return nctx.raise_io_error(
                    err,
                    IoOperation::Write,
                    "term/write-fd",
                    &msg,
                    fd.into(),
                );
            }
            nctx.return_(ret)
        }
    }

    #[scheme(name = "term/sigwinch-version")]
    pub fn term_sigwinch_version() -> u64 {
        nctx.return_(TERM_SIGWINCH_VERSION.load(Ordering::Relaxed))
    }

    #[scheme(name = "term/install-sigwinch-handler!")]
    pub fn term_install_sigwinch_handler() -> bool {
        unsafe {
            let mut action: libc::sigaction = std::mem::zeroed();
            action.sa_sigaction = term_sigwinch_handler as *const () as usize;
            action.sa_flags = 0;
            libc::sigemptyset(&mut action.sa_mask);

            if libc::sigaction(libc::SIGWINCH, &action, std::ptr::null_mut()) < 0 {
                let err = std::io::Error::last_os_error();
                let msg = err.to_string();
                return nctx.raise_io_error(
                    err,
                    IoOperation::Stat,
                    "term/install-sigwinch-handler!",
                    &msg,
                    Value::new(false),
                );
            }
            nctx.return_(true)
        }
    }

    #[scheme(name = "term/enable-raw-mode!")]
    pub fn term_enable_raw_mode() -> bool {
        let mut state = TERM_RAW_MODE_STATE.lock().expect("raw mode mutex poisoned");
        if state.is_some() {
            return nctx.return_(true);
        }

        unsafe {
            let fd = match open_tty_raw_fd() {
                Ok(fd) => fd,
                Err(err) => {
                    let msg = err.to_string();
                    return nctx.raise_io_error(
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
                return term_io_error(nctx, IoOperation::Stat, "term/enable-raw-mode!", fd.into());
            }

            let mut raw = saved;
            libc::cfmakeraw(&mut raw);
            if libc::tcsetattr(fd, libc::TCSANOW, &raw) < 0 {
                let _ = libc::close(fd);
                return term_io_error(nctx, IoOperation::Stat, "term/enable-raw-mode!", fd.into());
            }

            *state = Some((fd, saved));
            nctx.return_(true)
        }
    }

    #[scheme(name = "term/disable-raw-mode!")]
    pub fn term_disable_raw_mode() -> bool {
        let mut state = TERM_RAW_MODE_STATE.lock().expect("raw mode mutex poisoned");
        let Some((fd, saved)) = *state else {
            return nctx.return_(true);
        };

        unsafe {
            if libc::tcsetattr(fd, libc::TCSANOW, &saved) < 0 {
                return term_io_error(nctx, IoOperation::Stat, "term/disable-raw-mode!", fd.into());
            }

            if libc::close(fd) < 0 {
                return term_io_error(
                    nctx,
                    IoOperation::Close,
                    "term/disable-raw-mode!",
                    fd.into(),
                );
            }
        }

        *state = None;
        nctx.return_(true)
    }

    #[scheme(name = "term/raw-mode-enabled?")]
    pub fn term_raw_mode_enabled() -> bool {
        nctx.return_(
            TERM_RAW_MODE_STATE
                .lock()
                .expect("raw mode mutex poisoned")
                .is_some(),
        )
    }

    #[scheme(name = "term/terminal-size-list")]
    pub fn term_terminal_size_list() -> Value<'gc> {
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
                    nctx,
                    IoOperation::Stat,
                    "term/terminal-size-list",
                    fd.into(),
                );
            }

            let ctx = nctx.ctx;
            let mut result = Value::null();
            result = Value::cons(ctx, (size.ws_ypixel as i32).into(), result);
            result = Value::cons(ctx, (size.ws_xpixel as i32).into(), result);
            result = Value::cons(ctx, (size.ws_row as i32).into(), result);
            result = Value::cons(ctx, (size.ws_col as i32).into(), result);
            nctx.return_(result)
        }
    }

    #[scheme(name = "io/pread")]
    pub fn io_pread(
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        from: usize,
        nbytes: usize,
        offset: i64,
    ) -> isize {
        if from + nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
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

        unsafe {
            let ptr = buf.as_slice_mut_unchecked().as_mut_ptr().add(from) as *mut libc::c_void;
            let ret = libc::pread(fd, ptr, nbytes, offset);
            nctx.return_(ret)
        }
    }

    #[scheme(name = "io/pwrite")]
    pub fn io_pwrite(
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        from: usize,
        nbytes: usize,
        offset: i64,
    ) -> isize {
        if from + nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
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

        unsafe {
            let ptr = buf.as_slice().as_ptr().add(from) as *const libc::c_void;
            let ret = libc::pwrite(fd, ptr, nbytes, offset);
            nctx.return_(ret)
        }
    }

    #[scheme(name = "io/lseek")]
    pub fn io_lseek64(fd: i32, offset: i64, whence: i32) -> i64 {
        unsafe {
            let ret = libc::lseek(fd, offset, whence);
            nctx.return_(ret)
        }
    }

    #[scheme(name = "io/mkstemp")]
    pub fn io_mkstemp(template: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        if template.len() > 128 {
            return nctx.wrong_argument_violation(
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

        unsafe {
            let ret = libc::mkstemp(arr.as_mut_ptr() as *mut _);
            let newname = std::ffi::CStr::from_ptr(arr.as_ptr() as _).to_string_lossy();
            let ctx = nctx.ctx;
            let gname = Str::new(*ctx, &newname, true);
            let res = crate::list!(ctx, ret, gname);
            nctx.return_(res)
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
            return nctx.return_(-1);
        }

        unsafe {
            let cpath =
                CString::new(filename.to_string()).expect("filename should not contain null bytes");
            let ret = libc::open(cpath.as_ptr(), newflags | libc::O_NONBLOCK, mode);
            nctx.return_(ret)
        }
    }

    #[scheme(name = "syscall:close")]
    pub fn syscall_close(fd: i32) -> i32 {
        unsafe {
            let ret = libc::close(fd);

            nctx.return_(ret)
        }
    }

    #[scheme(name = "syscall:read")]
    pub fn syscall_read(fd: i32, buf: Gc<'gc, ByteVector>, nbytes: usize) -> isize {
        if nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "syscall:read",
                "buffer too small for read",
                None,
                None,
                3,
                &[fd.into(), buf.into(), nbytes.into_value(ctx)],
            );
        }

        loop {
            let ret = unsafe {
                let ptr = buf.as_slice_mut_unchecked().as_mut_ptr() as *mut libc::c_void;
                libc::read(fd, ptr, nbytes)
            };
            if ret != -1
                || (errno::errno().0 != libc::EAGAIN && errno::errno().0 != libc::EWOULDBLOCK)
            {
                return nctx.return_(ret);
            }
            nctx.ctx.outside_gc_world(|| blocking_poll_read(fd));
        }
    }

    #[scheme(name = "syscall:write")]
    pub fn syscall_write(fd: i32, buf: Gc<'gc, ByteVector>, nbytes: usize, offset: usize) -> isize {
        if nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "syscall:write",
                "buffer too small for write",
                None,
                None,
                3,
                &[fd.into(), buf.into(), nbytes.into_value(ctx)],
            );
        }

        loop {
            let ret = unsafe {
                let ptr = buf.as_slice().as_ptr().wrapping_add(offset) as *const libc::c_void;
                libc::write(fd, ptr, nbytes)
            };
            if ret != -1
                || (errno::errno().0 != libc::EAGAIN && errno::errno().0 != libc::EWOULDBLOCK)
            {
                return nctx.return_(ret);
            }
            nctx.ctx.outside_gc_world(|| blocking_poll_write(fd));
        }
    }

    #[scheme(name = "syscall:lseek")]
    pub fn syscall_lseek(fd: i32, offset: i64, whence: i32) -> i64 {
        let whence = match whence {
            0 => libc::SEEK_SET,
            1 => libc::SEEK_CUR,
            2 => libc::SEEK_END,
            _ => return nctx.return_(-1),
        };
        loop {
            let ret = unsafe { libc::lseek(fd, offset, whence) };
            if ret != -1
                || (errno::errno().0 != libc::EAGAIN && errno::errno().0 != libc::EWOULDBLOCK)
            {
                return nctx.return_(ret);
            }
            nctx.ctx.outside_gc_world(|| blocking_poll_write(fd));
        }
    }

    #[scheme(name = "syscall:pollinput")]
    pub fn syscall_pollinput(fd: i32) -> i32 {
        unsafe {
            let mut fds = libc::pollfd {
                fd,
                events: libc::POLLIN,
                revents: 0,
            };
            let ret = libc::poll(&mut fds as *mut libc::pollfd, 1, 0);
            if ret > 0 && (fds.revents & (libc::POLLIN | libc::POLLHUP | libc::POLLERR)) != 0 {
                nctx.return_(1)
            } else {
                nctx.return_(0)
            }
        }
    }

    #[scheme(name = "syscall:unlink")]
    pub fn syscall_unlink(filename: Gc<'gc, Str<'gc>>) -> i32 {
        let cpath =
            CString::new(filename.to_string()).expect("filename should not contain null bytes");
        unsafe {
            let ret = libc::unlink(cpath.as_ptr());
            nctx.return_(ret)
        }
    }

    #[scheme(name = "syscall:rename")]
    pub fn syscall_rename(old_filename: Gc<'gc, Str<'gc>>, new_filename: Gc<'gc, Str<'gc>>) -> i32 {
        let cold = CString::new(old_filename.to_string())
            .expect("old filename should not contain null bytes");
        let cnew = CString::new(new_filename.to_string())
            .expect("new filename should not contain null bytes");
        unsafe {
            let ret = libc::rename(cold.as_ptr(), cnew.as_ptr());
            nctx.return_(ret)
        }
    }

    #[scheme(name = "syscall:mtime")]
    pub fn syscall_mtime(filename: Gc<'gc, Str<'gc>>, vbuf: Gc<'gc, Vector<'gc>>) -> i32 {
        let path = filename.to_string();

        unsafe {
            let mut buf = std::mem::zeroed::<libc::stat>();
            let cpath = CString::new(path).expect("path should not contain null bytes");
            let ret = libc::stat(cpath.as_ptr(), &mut buf);
            if ret != 0 {
                return nctx.return_(-1);
            }

            let tm = libc::localtime(&buf.st_mtime);
            let wbuf = Gc::write(*nctx.ctx, vbuf);
            wbuf[0]
                .unlock()
                .set((1900 + (*tm).tm_year).into_value(nctx.ctx));
            wbuf[1]
                .unlock()
                .set((1 + (*tm).tm_mon).into_value(nctx.ctx));
            wbuf[2].unlock().set((*tm).tm_mday.into_value(nctx.ctx));
            wbuf[3].unlock().set((*tm).tm_hour.into_value(nctx.ctx));
            wbuf[4].unlock().set((*tm).tm_min.into_value(nctx.ctx));
            wbuf[5].unlock().set((*tm).tm_sec.into_value(nctx.ctx));
            nctx.return_(0)
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
        unsafe {
            let ret = libc::access(cpath.as_ptr(), rmode);
            nctx.return_(ret)
        }
    }

    #[scheme(name = "syscall:recv")]
    pub fn syscall_recv(fd: i32, buf: Gc<'gc, ByteVector>, nbytes: usize, flags: i32) -> isize {
        if nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "syscall:recv",
                "buffer too small for recv",
                None,
                None,
                3,
                &[fd.into(), buf.into(), nbytes.into_value(ctx)],
            );
        }

        loop {
            let ret = unsafe {
                let ptr = buf.as_slice_mut_unchecked().as_mut_ptr() as *mut libc::c_void;
                libc::recv(fd, ptr, nbytes, flags)
            };
            if ret != -1
                || (errno::errno().0 != libc::EAGAIN && errno::errno().0 != libc::EWOULDBLOCK)
            {
                return nctx.return_(ret);
            }
            nctx.ctx.outside_gc_world(|| blocking_poll_read(fd));
        }
    }

    #[scheme(name = "syscall:send")]
    pub fn syscall_send(fd: i32, buf: Gc<'gc, ByteVector>, nbytes: usize, flags: i32) -> isize {
        if nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "syscall:send",
                "buffer too small for send",
                None,
                None,
                3,
                &[fd.into(), buf.into(), nbytes.into_value(ctx)],
            );
        }

        loop {
            let ret = unsafe {
                let ptr = buf.as_slice().as_ptr() as *const libc::c_void;
                libc::send(fd, ptr, nbytes, flags)
            };
            if ret != -1
                || (errno::errno().0 != libc::EAGAIN && errno::errno().0 != libc::EWOULDBLOCK)
            {
                return nctx.return_(ret);
            }
            nctx.ctx.outside_gc_world(|| blocking_poll_write(fd));
        }
    }

    #[scheme(name = "process-environment->alist")]
    pub fn process_environment_to_alist() -> Value<'gc> {
        let ctx = nctx.ctx;
        let mut alist = Value::null();
        for (key, value) in std::env::vars() {
            let key_str = Str::new(*ctx, &key, true);
            let value_str = Str::new(*ctx, &value, true);
            let pair = Value::cons(ctx, key_str.into(), value_str.into());
            alist = Value::cons(ctx, pair, alist);
        }
        alist = alist.list_reverse(ctx);
        nctx.return_(alist)
    }

    #[scheme(name = "system")]
    pub fn system(command: Gc<'gc, Str<'gc>>) -> i32 {
        let ccommand =
            CString::new(command.to_string()).expect("command should not contain null bytes");
        unsafe {
            let ret = libc::system(ccommand.as_ptr());
            if ret != -1 {
                if libc::WIFEXITED(ret) {
                    nctx.return_(libc::WEXITSTATUS(ret))
                } else if libc::WIFSIGNALED(ret) {
                    nctx.return_(-libc::WTERMSIG(ret))
                } else {
                    nctx.return_(-1)
                }
            } else {
                let errno = ::errno::errno();

                let message = format!("system() failed: {errno}");
                let errno_code = errno.0;

                nctx.raise_error("system", &message, &[errno_code.into(), command.into()])
            }
        }
    }

    #[scheme(name = "errno/string")]
    pub fn errno_string() -> (i32, Gc<'gc, Str<'gc>>) {
        let errno = ::errno::errno();
        let message = errno.to_string();
        let ctx = nctx.ctx;
        nctx.return_((errno.0, Str::new(*ctx, &message, true)))
    }

    #[scheme(name = "acquire-lockfile")]
    pub fn acquire_lockfile(path: Gc<'gc, Str<'gc>>, wait: Option<bool>) -> i32 {
        let wait = wait.unwrap_or(true);
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
            return nctx.raise_io_error(
                err,
                IoOperation::Open,
                "acquire-lockfile",
                &error,
                path.into(),
            );
        }

        if unsafe { libc::flock(fd, libc::LOCK_EX | if wait { 0 } else { libc::LOCK_NB }) } != 0 {
            let err = std::io::Error::last_os_error();
            let error = err.to_string();
            return nctx.raise_io_error(
                err,
                IoOperation::Open,
                "acquire-lockfile",
                &error,
                path.into(),
            );
        }

        nctx.return_(fd)
    }

    #[scheme(name = "release-lockfile")]
    pub fn release_lockfile(fd: i32) -> bool {
        if unsafe { libc::flock(fd, libc::LOCK_UN) } != 0 {
            let err = std::io::Error::last_os_error();
            let error = err.to_string();
            return nctx.raise_io_error(
                err,
                IoOperation::Close,
                "release-lockfile",
                &error,
                Value::new(false),
            );
        }

        if unsafe { libc::close(fd) } != 0 {
            let err = std::io::Error::last_os_error();
            let error = err.to_string();
            return nctx.raise_io_error(
                err,
                IoOperation::Close,
                "release-lockfile",
                &error,
                Value::new(false),
            );
        }

        nctx.return_(true)
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
                return nctx.raise_io_error(
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
        let ctx = nctx.ctx;
        nctx.return_(if pipe_stderr {
            crate::list!(ctx, ifd, efd, ofd, pid)
        } else {
            crate::list!(ctx, ifd, ofd, pid)
        })
    }

    #[scheme(name = "%process-wait")]
    pub fn process_wait(pid: i32) -> i32 {
        unsafe {
            let mut status: i32 = 0;
            let ret = libc::waitpid(pid, &mut status as *mut i32, 0);
            if ret == -1 {
                let err = std::io::Error::last_os_error();
                let error = err.to_string();
                return nctx.raise_io_error(
                    err,
                    IoOperation::Read,
                    "%process-wait",
                    &error,
                    pid.into(),
                );
            }

            if libc::WIFEXITED(status) {
                nctx.return_(libc::WEXITSTATUS(status))
            } else if libc::WIFSIGNALED(status) {
                nctx.return_(-libc::WTERMSIG(status))
            } else {
                nctx.return_(-1)
            }
        }
    }

    /* polling primitives */
    #[scheme(name = "make-poller")]
    /// Create a new poller object instance. This can be used
    /// to perform non-blocking I/O operations.
    pub fn make_poller() -> Gc<'gc, Poller> {
        let inner = match polling::Poller::new() {
            Ok(poller) => poller,
            Err(err) => {
                return nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "make-poller",
                    "failed to create poller",
                    Value::new(false),
                );
            }
        };
        let poller = Gc::new_with_header_word(*ctx, Poller { inner }, poller_header_word());

        nctx.return_(poller)
    }

    /// Add a file descriptor to the poller with the specified flags.
    ///
    /// Arguments:
    /// - `key`: A unique identifier for the file descriptor.
    /// - `fd`: The file descriptor to monitor.
    /// - `flags`: The events to monitor (e.g., readable, writable).
    #[scheme(name = "poller-add!")]
    pub fn poller_add(poller: Gc<'gc, Poller>, key: usize, fd: i32, flags: i32) -> bool {
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

        unsafe {
            match poller.inner.add(&fd, event) {
                Ok(()) => nctx.return_(true),
                Err(err) => {
                    let error = err.to_string();
                    nctx.raise_io_error(
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
    ) -> bool {
        let fd = unsafe { std::os::fd::BorrowedFd::borrow_raw(fd) };
        let mode = match mode {
            EONESHOT => polling::PollMode::Oneshot,
            ELEVEL => polling::PollMode::Level,
            EEDGE => polling::PollMode::Edge,
            EEDGEONESHOT => polling::PollMode::EdgeOneshot,
            _ => {
                return nctx.wrong_argument_violation(
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

        unsafe {
            match poller.inner.add_with_mode(&fd, event, mode) {
                Ok(()) => nctx.return_(true),
                Err(err) => {
                    let error = err.to_string();
                    nctx.raise_io_error(
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
    pub fn poller_delete(poller: Gc<'gc, Poller>, fd: i32) -> bool {
        let fd = unsafe { std::os::fd::BorrowedFd::borrow_raw(fd) };
        {
            match poller.inner.delete(fd) {
                Ok(()) => nctx.return_(true),
                Err(err) => {
                    let error = err.to_string();
                    nctx.raise_io_error(
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
    pub fn poller_modify(poller: Gc<'gc, Poller>, fd: i32, flags: i32) -> bool {
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
                Ok(()) => nctx.return_(true),
                Err(err) => {
                    let error = err.to_string();
                    nctx.raise_io_error(
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
    ) -> bool {
        let fd = unsafe { std::os::fd::BorrowedFd::borrow_raw(fd) };
        let mode = match mode {
            EONESHOT => polling::PollMode::Oneshot,
            ELEVEL => polling::PollMode::Level,
            EEDGE => polling::PollMode::Edge,
            EEDGEONESHOT => polling::PollMode::EdgeOneshot,
            _ => {
                return nctx.wrong_argument_violation(
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
                Ok(()) => nctx.return_(true),
                Err(err) => {
                    let error = err.to_string();
                    nctx.raise_io_error(
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
        let ctx = nctx.ctx;
        let timeout = timeout.map(std::time::Duration::from_micros);
        let poller_ptr = Gc::as_ptr(poller);
        let wait_outcome = ctx.outside_gc_world(|| {
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
            nctx.return_(ls)
        } else {
            let err = os_error.unwrap_or(i32::MAX);
            nctx.raise_error(
                "poller-wait",
                "failed to wait for events",
                &[err.into_value(ctx)],
            )
        }
    }

    #[scheme(name = "poller-notify")]
    /// Wakes up current or future `poller-wait` invocation.
    pub fn poller_notify(poller: Gc<'gc, Poller>) -> bool {
        match poller.inner.notify() {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Write,
                    "poller-notify",
                    &error,
                    Value::new(false),
                )
            }
        }
    }
}

pub fn init_io<'gc>(ctx: Context<'gc>) {
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

    let host_features_tab = HashTable::new(*ctx, HashTableType::Eq, 64, 0.80);
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
    class_header_word(ClassId::new(builtin_class_ids::POLLER).unwrap())
}

unsafe impl ClassTagged for Poller {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::POLLER];
    const TYPE_NAME: &'static str = "poller";
}

unsafe impl Trace for Poller {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        let _ = visitor;
    }

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
    unsafe {
        let mut readfs = [rustix::event::FdSetElement::default(); 1];
        rustix::event::fd_set_insert(&mut readfs, fd);
        let _ = rustix::event::select(readfs.len() as _, Some(&mut readfs), None, None, None);
    }
}

fn blocking_poll_write(fd: i32) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Scheme;

    #[test]
    fn poller_allocates_with_class_only_header() {
        Scheme::new_uninit().enter(|ctx| {
            let poller = Poller {
                inner: polling::Poller::new().expect("create poller"),
            };
            let poller = Gc::new_with_header_word(*ctx, poller, poller_header_word());

            assert_eq!(
                poller.as_gcobj().header().class_id(),
                ClassId::new(builtin_class_ids::POLLER).unwrap()
            );
        });
    }
}
