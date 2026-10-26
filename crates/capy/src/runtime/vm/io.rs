use crate::heap::Space;
use crate::heap::object::{ClassId, builtin_class_ids, class_header_word};
use crate::heap::{Visitor, WeakProcessor};
use crate::prelude::*;
use crate::runtime::fasl::{Reader, Writer};
use crate::runtime::sched::blocking;
use crate::runtime::sched::preempt::retry_eintr;
use crate::runtime::sched::reactor::{self, Dir};
use crate::runtime::sched::scheduler::{Woken, current};
use crate::static_symbols;
use rustix::fd::AsRawFd;
use std::ffi::CString;
use std::sync::{
    Arc, Mutex, Once,
    atomic::{AtomicI32, Ordering},
};
static_symbols!(
    SYM_INPUT = "input"
    SYM_OUTPUT = "output"
    SYM_ERROR = "error"
    SYM_BINARY = "binary"
    SYM_TEXTUAL = "textual"
);

/// Self-pipe every installed signal handler writes to: `[read end, write end]`,
/// -1 until `signal/fd` first opens it. Each signal arrives as one byte holding
/// its number.
static SIGNAL_PIPE: [AtomicI32; 2] = [AtomicI32::new(-1), AtomicI32::new(-1)];
/// Signals with a live handler, mapped to the action they replaced.
static SIGNAL_PREVIOUS: Mutex<Vec<(i32, libc::sigaction)>> = Mutex::new(Vec::new());
static TERM_RAW_MODE_STATE: Mutex<Option<(i32, libc::termios)>> = Mutex::new(None);

extern "C" fn signal_handler(signo: libc::c_int) {
    let fd = SIGNAL_PIPE[1].load(Ordering::Relaxed);
    if fd < 0 {
        return;
    }
    // Async-signal-safe. A full pipe already holds a wake-up, and errno must
    // survive for the code the signal interrupted.
    // SAFETY: `fd` is the non-blocking write end opened by `open_signal_pipe`.
    unsafe {
        let errno = errno_location();
        let saved = *errno;
        let byte = signo as u8;
        libc::write(fd, (&byte as *const u8).cast(), 1);
        *errno = saved;
    }
}

/// Path of the terminal device to open. The device behind stdin, stdout or
/// stderr is preferred over the `/dev/tty` alias, which kqueue and poll(2)
/// refuse on macOS, so threads could not wait on it. `/dev/tty` remains the
/// fallback for a process whose stdio is all redirected.
fn controlling_tty_path() -> CString {
    let mut name = [0 as libc::c_char; 256];
    for fd in 0..=2 {
        // SAFETY: `name` is writable for the length passed.
        let named = unsafe { libc::ttyname_r(fd, name.as_mut_ptr(), name.len()) } == 0;
        if named {
            // SAFETY: ttyname_r wrote a NUL-terminated string into `name`.
            return unsafe { std::ffi::CStr::from_ptr(name.as_ptr()) }.to_owned();
        }
    }
    CString::new("/dev/tty").expect("static tty path")
}

/// Signals the scheduler, stack guards and profiler rely on, plus the ones the
/// kernel will not let a process catch.
fn signal_reserved(signo: i32) -> bool {
    matches!(
        signo,
        libc::SIGKILL
            | libc::SIGSTOP
            | libc::SIGURG
            | libc::SIGSEGV
            | libc::SIGBUS
            | libc::SIGILL
            | libc::SIGFPE
            | libc::SIGPROF
    ) || signo <= 0
        || signo > 255
}

#[cfg(any(target_os = "macos", target_os = "ios", target_os = "freebsd"))]
unsafe fn errno_location() -> *mut libc::c_int {
    unsafe { libc::__error() }
}

#[cfg(not(any(target_os = "macos", target_os = "ios", target_os = "freebsd")))]
unsafe fn errno_location() -> *mut libc::c_int {
    unsafe { libc::__errno_location() }
}

fn open_signal_pipe() -> std::io::Result<i32> {
    let existing = SIGNAL_PIPE[0].load(Ordering::Acquire);
    if existing >= 0 {
        return Ok(existing);
    }
    let mut fds = [0i32; 2];
    // SAFETY: `fds` has room for the two descriptors pipe(2) writes.
    if unsafe { libc::pipe(fds.as_mut_ptr()) } < 0 {
        return Err(std::io::Error::last_os_error());
    }
    for fd in fds {
        // SAFETY: both descriptors were just opened and are owned here.
        unsafe {
            libc::fcntl(
                fd,
                libc::F_SETFL,
                libc::fcntl(fd, libc::F_GETFL) | libc::O_NONBLOCK,
            );
            libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC);
        }
    }
    SIGNAL_PIPE[1].store(fds[1], Ordering::Relaxed);
    SIGNAL_PIPE[0].store(fds[0], Ordering::Release);
    Ok(fds[0])
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
    let path = controlling_tty_path();
    // SAFETY: FFI: path is a valid NUL-terminated string with valid flags
    let fd = unsafe { retry_eintr(|| libc::open(path.as_ptr(), libc::O_RDWR | libc::O_CLOEXEC)) };
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
        match crate::utils::flags::home_directory() {
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
        match crate::utils::flags::var(&var.to_string()) {
            Some(s) => Str::new(ctx, &s, true).into(),
            None => Value::new(false),
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
            retry_eintr(|| libc::open(cpath.as_ptr(), oflag, pmode))
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
            let path = controlling_tty_path();
            let ret = retry_eintr(|| libc::open(path.as_ptr(), flags));
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
        reactor::fd_closed(fd);
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
            let ret = retry_eintr(|| libc::read(fd, ptr, nbytes));
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
            let ret = retry_eintr(|| libc::read(fd, ptr, count));
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
            let ret = retry_eintr(|| libc::write(fd, ptr, nbytes));
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
            let ret = retry_eintr(|| libc::write(fd, ptr, count));
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

    /// Return the read end of the signal self-pipe, creating it on first use.
    /// The descriptor is non-blocking and receives one byte (the signal
    /// number) per delivered signal.
    #[scheme(name = "signal/fd")]
    pub fn signal_fd() -> Value<'gc> {
        match open_signal_pipe() {
            Ok(fd) => fd.into(),
            Err(err) => {
                let msg = err.to_string();
                call.raise_io_error(err, IoOperation::Open, "signal/fd", &msg, Value::new(false))
            }
        }
    }

    /// Route `signo` into the self-pipe. Returns #f for signals the runtime
    /// owns or that cannot be caught. Installing twice is a no-op.
    #[scheme(name = "signal/install!")]
    pub fn signal_install(signo: i32) -> Value<'gc> {
        if signal_reserved(signo) || open_signal_pipe().is_err() {
            return Value::new(false);
        }
        let mut previous = SIGNAL_PREVIOUS.lock().unwrap();
        if previous.iter().any(|(s, _)| *s == signo) {
            return Value::new(true);
        }
        // SAFETY: a zeroed sigaction is valid and the handler is async-signal-safe.
        unsafe {
            let mut action: libc::sigaction = std::mem::zeroed();
            let mut old: libc::sigaction = std::mem::zeroed();
            action.sa_sigaction = signal_handler as *const () as usize;
            action.sa_flags = libc::SA_RESTART;
            libc::sigemptyset(&mut action.sa_mask);
            if libc::sigaction(signo, &action, &mut old) < 0 {
                return Value::new(false);
            }
            previous.push((signo, old));
        }
        Value::new(true)
    }

    /// The platform's number for the `index`th name in `(capy signal)`'s
    /// table, or #f past its end.
    #[scheme(name = "signal/number")]
    pub fn signal_number(index: i32) -> Value<'gc> {
        const NUMBERS: [i32; 11] = [
            libc::SIGHUP,
            libc::SIGINT,
            libc::SIGQUIT,
            libc::SIGTERM,
            libc::SIGWINCH,
            libc::SIGUSR1,
            libc::SIGUSR2,
            libc::SIGALRM,
            libc::SIGCHLD,
            libc::SIGPIPE,
            libc::SIGCONT,
        ];
        match usize::try_from(index).ok().and_then(|i| NUMBERS.get(i)) {
            Some(n) => (*n).into(),
            None => Value::new(false),
        }
    }

    /// Restore the action `signal/install!` replaced.
    #[scheme(name = "signal/uninstall!")]
    pub fn signal_uninstall(signo: i32) -> Value<'gc> {
        let mut previous = SIGNAL_PREVIOUS.lock().unwrap();
        let Some(at) = previous.iter().position(|(s, _)| *s == signo) else {
            return Value::new(false);
        };
        let (_, old) = previous.swap_remove(at);
        // SAFETY: `old` is the action the kernel returned when installing.
        unsafe { libc::sigaction(signo, &old, std::ptr::null_mut()) };
        Value::new(true)
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
                let path = controlling_tty_path();
                let fd =
                    retry_eintr(|| libc::open(path.as_ptr(), libc::O_RDONLY | libc::O_CLOEXEC));
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
            let ret = retry_eintr(|| libc::pread(fd, ptr, nbytes, offset));
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
            let ret = retry_eintr(|| libc::open(cpath.as_ptr(), newflags | libc::O_NONBLOCK, mode));
            ret
        }
    }

    #[scheme(name = "syscall:close")]
    pub fn syscall_close(fd: i32) -> i32 {
        reactor::fd_closed(fd);
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

        if let Some(cx) = current() {
            let scope = RootScope::new(ctx);
            let buf_root = scope.root(buf.into());
            let (ctx, result) = virtual_thread_io(ctx, &cx, fd, Dir::Read, || {
                let mut buf: Gc<'gc, ByteVector> = buf_root.get().downcast();
                // SAFETY: the buffer is rooted and no collection runs between
                // this attempt and the syscall
                let ptr = unsafe {
                    buf.as_slice_mut_unchecked().as_mut_ptr().add(start) as *mut libc::c_void
                };
                // SAFETY: FFI: the range is inside the buffer, checked above.
                let ret = unsafe { libc::read(fd, ptr, nbytes) };
                syscall_result(ret)
            });
            return io_result(ctx, result);
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

        if let Some(cx) = current() {
            let scope = RootScope::new(ctx);
            let buf_root = scope.root(buf.into());
            let (ctx, result) = virtual_thread_io(ctx, &cx, fd, Dir::Write, || {
                let buf: Gc<'gc, ByteVector> = buf_root.get().downcast();
                let ptr = buf.as_slice().as_ptr().wrapping_add(offset) as *const libc::c_void;
                // SAFETY: FFI: the range is inside the buffer, checked above.
                let ret = unsafe { libc::write(fd, ptr, nbytes) };
                syscall_result(ret)
            });
            return io_result(ctx, result);
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
        // SAFETY: FFI: fd is valid, whence is a valid seek origin. Seeking never
        // waits for I/O, so there is nothing to park on.
        unsafe { libc::lseek(fd, offset, whence) }
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
            let ret = retry_eintr(|| libc::poll(&mut fds as *mut libc::pollfd, 1, 0));
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
            let ret = retry_eintr(|| libc::stat(cpath.as_ptr(), &mut buf));
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
            let ret = retry_eintr(|| libc::access(cpath.as_ptr(), rmode));
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

        if let Some(cx) = current() {
            let scope = RootScope::new(ctx);
            let buf_root = scope.root(buf.into());
            let (ctx, result) = virtual_thread_io(ctx, &cx, fd, Dir::Read, || {
                let mut buf: Gc<'gc, ByteVector> = buf_root.get().downcast();
                // SAFETY: as in `syscall:read`.
                let ptr = unsafe { buf.as_slice_mut_unchecked().as_mut_ptr() as *mut libc::c_void };
                // SAFETY: FFI: the range is inside the buffer, checked above.
                let ret = unsafe { libc::recv(fd, ptr, nbytes, flags) };
                syscall_result(ret)
            });
            return io_result(ctx, result);
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

        if let Some(cx) = current() {
            let scope = RootScope::new(ctx);
            let buf_root = scope.root(buf.into());
            let (ctx, result) = virtual_thread_io(ctx, &cx, fd, Dir::Write, || {
                let buf: Gc<'gc, ByteVector> = buf_root.get().downcast();
                let ptr = buf.as_slice().as_ptr() as *const libc::c_void;
                // SAFETY: FFI: the range is inside the buffer, checked above.
                let ret = unsafe { libc::send(fd, ptr, nbytes, flags) };
                syscall_result(ret)
            });
            return io_result(ctx, result);
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
        for (key, value) in crate::utils::flags::vars() {
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
        // A thread waits for the child on the blocking pool, so its worker
        // keeps running.
        let (ret, errno_code) = match current() {
            Some(cx) => blocking::run(ctx, &cx, move || run_system(&ccommand))
                .1
                .unwrap_or_else(|killed| killed.unwind()),
            None => run_system(&ccommand),
        };
        if ret != -1 {
            if libc::WIFEXITED(ret) {
                (libc::WEXITSTATUS(ret)).into()
            } else if libc::WIFSIGNALED(ret) {
                (-libc::WTERMSIG(ret)).into()
            } else {
                (-1).into()
            }
        } else {
            let message = format!("system() failed: {}", errno::Errno(errno_code));
            call.raise_error("system", &message, &[errno_code.into(), command.into()])
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
            retry_eintr(|| {
                libc::open(
                    CString::new(path.to_string())
                        .expect("path should not contain null bytes")
                        .as_ptr(),
                    libc::O_CREAT,
                    (libc::S_IRUSR | libc::S_IWUSR) as libc::c_uint,
                )
            })
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
        if unsafe {
            retry_eintr(|| libc::flock(fd, libc::LOCK_EX | if wait { 0 } else { libc::LOCK_NB }))
        } != 0
        {
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
        let outcome = match current() {
            Some(cx) => blocking::run(ctx, &cx, move || wait_child(pid))
                .1
                .unwrap_or_else(|killed| killed.unwind()),
            None => wait_child(pid),
        };
        match outcome {
            Ok(code) => code.into(),
            Err(err) => {
                let error = err.to_string();
                call.raise_io_error(err, IoOperation::Read, "%process-wait", &error, pid.into())
            }
        }
    }

    /// Park the calling thread until `fd` is readable. Returns `#t` when
    /// it is, and raises when the fd is closed while waiting.
    #[scheme(name = "%await-readable")]
    pub fn await_readable(fd: i32) -> Value<'gc> {
        match await_native(ctx, fd, Dir::Read) {
            (_, Ok(())) => Value::new(true),
            (_, Err(errno)) => {
                let err = std::io::Error::from_raw_os_error(errno);
                let msg = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Read,
                    "%await-readable",
                    &msg,
                    Value::new(false),
                )
            }
        }
    }

    /// Park the calling thread until `fd` is writable.
    #[scheme(name = "%await-writable")]
    pub fn await_writable(fd: i32) -> Value<'gc> {
        match await_native(ctx, fd, Dir::Write) {
            (_, Ok(())) => Value::new(true),
            (_, Err(errno)) => {
                let err = std::io::Error::from_raw_os_error(errno);
                let msg = err.to_string();
                call.raise_io_error(
                    err,
                    IoOperation::Write,
                    "%await-writable",
                    &msg,
                    Value::new(false),
                )
            }
        }
    }

    /// Park the calling thread until `fd` is ready for `events` (1 readable,
    /// 2 writable, 3 both), or until `timeout-ms` milliseconds have passed.
    /// `timeout-ms` is a non-negative integer, or `#f` for no deadline.
    ///
    /// Returns 0 when the fd is ready, 1 on timeout, and 2 when the fd is
    /// closed while waiting or was never open. An fd that cannot be polled,
    /// such as a regular file, counts as ready. Outside a thread this blocks
    /// the thread in `poll`.
    #[scheme(name = "%await-fd-timeout")]
    pub fn await_fd_timeout(fd: i32, events: i32, timeout_ms: Value<'gc>) -> Value<'gc> {
        let (read, write) = match events {
            1 => (true, false),
            2 => (false, true),
            3 => (true, true),
            _ => {
                let irritant = Value::from_i32(events);
                return call.raise_error(
                    "%await-fd-timeout",
                    "events must be 1 (readable), 2 (writable) or 3 (both)",
                    &[irritant],
                );
            }
        };
        let deadline = if timeout_ms.is_bool() && !timeout_ms.as_bool() {
            None
        } else {
            match timeout_ms.int32() {
                Some(ms) if ms >= 0 => {
                    Some(std::time::Instant::now() + std::time::Duration::from_millis(ms as u64))
                }
                _ => {
                    return call.raise_error(
                        "%await-fd-timeout",
                        "timeout must be a non-negative integer or #f",
                        &[timeout_ms],
                    );
                }
            }
        };
        let (_, code) = await_fd_timed(ctx, fd, read, write, deadline);
        Value::from_i32(code)
    }

    /// Park the calling thread for `microseconds`. Other threads keep
    /// the worker. An interrupt runs its handler, then the sleep resumes for
    /// the time that is left.
    #[scheme(name = "%sleep")]
    pub fn sleep_micros(microseconds: u64) -> Value<'gc> {
        let duration = std::time::Duration::from_micros(microseconds);
        let Some(cx) = current() else {
            ctx.call_in_native(|| std::thread::sleep(duration));
            return Value::new(true);
        };
        let deadline = std::time::Instant::now() + duration;
        let mut ctx = ctx;
        while std::time::Instant::now() < deadline {
            let (next, woken) = cx.sleep_until(ctx, deadline);
            ctx = next;
            let woken = woken.unwrap_or_else(|killed| killed.unwind());
            if let Woken::Interrupted = woken {
                crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
            }
        }
        Value::new(true)
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
        let poller = ctx.allocate_in(
            Space::Los,
            Poller {
                inner: Arc::new(inner),
            },
            poller_header_word(),
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
    /// Waits for at least one I/O event and returns a list of events. A
    /// thread waits on the blocking pool, so its worker keeps running.
    pub fn poller_wait(poller: Gc<'gc, Poller>, timeout: Option<u64>) -> Value<'gc> {
        let timeout = timeout.map(std::time::Duration::from_micros);
        // The wait owns an `Arc` to the poller, so it touches no heap object.
        let inner = poller.inner.clone();
        let (ctx, wait_outcome) = match current() {
            Some(cx) => {
                let (next, outcome) =
                    blocking::run(ctx, &cx, move || wait_for_events(&inner, timeout));
                (next, outcome.unwrap_or_else(|killed| killed.unwind()))
            }
            None => (ctx, ctx.call_in_native(|| wait_for_events(&inner, timeout))),
        };

        match wait_outcome {
            Ok(events) => {
                let mut ls = Value::null();
                for (key, flags) in events {
                    ls = Value::cons(
                        ctx,
                        Value::cons(ctx, key.into_value(ctx), flags.into_value(ctx)),
                        ls,
                    );
                }
                ls
            }
            Err(err) => call.raise_error(
                "poller-wait",
                "failed to wait for events",
                &[err.into_value(ctx)],
            ),
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
    export_term_primitive!("signal/fd", make_static_closure_signal_fd);
    export_term_primitive!("signal/install!", make_static_closure_signal_install);
    export_term_primitive!("signal/number", make_static_closure_signal_number);
    export_term_primitive!("signal/uninstall!", make_static_closure_signal_uninstall);
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
    inner: Arc<polling::Poller>,
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
    blocking_select(fd, Dir::Read);
}

fn blocking_poll_write(fd: i32) {
    blocking_select(fd, Dir::Write);
}

/// Block in select(2) until `fd` is ready.
fn blocking_select(fd: i32, dir: Dir) {
    use rustix::event::{FdSetElement, fd_set_insert, fd_set_num_elements, select};
    let nfds = fd + 1;
    let mut set = vec![FdSetElement::default(); fd_set_num_elements(1, nfds)];
    fd_set_insert(&mut set, fd);
    let (read, write) = match dir {
        Dir::Read => (Some(&mut set[..]), None),
        Dir::Write => (None, Some(&mut set[..])),
    };
    // SAFETY: `set` holds `nfds` bits and `fd` is below that.
    let _ = unsafe { select(nfds, read, write, None, None) };
}

/// Map a raw syscall return to `Ok(bytes)` or the errno of a `-1` return.
fn syscall_result(ret: isize) -> Result<isize, i32> {
    if ret < 0 {
        Err(errno::errno().0)
    } else {
        Ok(ret)
    }
}

fn io_result<'gc>(ctx: MutationContext<'gc>, result: Result<isize, i32>) -> Value<'gc> {
    match result {
        Ok(n) => n.into_value(ctx),
        Err(errno) => {
            errno::set_errno(errno::Errno(errno));
            (-1isize).into_value(ctx)
        }
    }
}

const STDIO_FDS: usize = 3;

static STDIO_FLAGS: [AtomicI32; STDIO_FDS] = [const { AtomicI32::new(-1) }; STDIO_FDS];
static STDIO_RESTORE: Once = Once::new();

extern "C" fn restore_stdio_flags() {
    for (fd, flags) in STDIO_FLAGS.iter().enumerate() {
        let original = flags.load(Ordering::SeqCst);
        if original >= 0 {
            // SAFETY: FFI: fcntl on a descriptor number is always safe to call.
            unsafe { libc::fcntl(fd as i32, libc::F_SETFL, original) };
        }
    }
}

//
fn ensure_nonblocking(fd: i32) {
    // SAFETY: FFI: fcntl on a descriptor number is always safe to call. A bad
    // descriptor makes it fail, which leaves the descriptor unchanged.
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        if flags < 0 || flags & libc::O_NONBLOCK != 0 {
            return;
        }
        if let Some(slot) = STDIO_FLAGS.get(fd as usize) {
            let _ = slot.compare_exchange(-1, flags, Ordering::SeqCst, Ordering::SeqCst);
            STDIO_RESTORE.call_once(|| {
                libc::atexit(restore_stdio_flags);
            });
        }
        libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
    }
}

fn run_system(command: &std::ffi::CStr) -> (i32, i32) {
    static ACTIVE: parking_lot::Mutex<(usize, [bool; STDIO_FDS])> =
        parking_lot::Mutex::new((0, [false; STDIO_FDS]));
    {
        let mut active = ACTIVE.lock();
        if active.0 == 0 {
            active.1 = stdio_blocking_for_child();
        }
        active.0 += 1;
    }
    // SAFETY: FFI: `command` is a valid NUL-terminated string.
    let ret = unsafe { libc::system(command.as_ptr()) };
    let errno = errno::errno().0;
    {
        let mut active = ACTIVE.lock();
        active.0 -= 1;
        if active.0 == 0 {
            restore_stdio_nonblocking(std::mem::take(&mut active.1));
        }
    }
    (ret, errno)
}

/// Clear `O_NONBLOCK` on the stdio descriptors this process changed, and
/// report which ones it cleared.
fn stdio_blocking_for_child() -> [bool; STDIO_FDS] {
    let mut changed = [false; STDIO_FDS];
    for (fd, flag) in changed.iter_mut().enumerate() {
        if STDIO_FLAGS[fd].load(Ordering::SeqCst) < 0 {
            continue;
        }
        // SAFETY: FFI: fcntl on a descriptor number is always safe to call.
        unsafe {
            let flags = libc::fcntl(fd as i32, libc::F_GETFL);
            if flags >= 0 && flags & libc::O_NONBLOCK != 0 {
                libc::fcntl(fd as i32, libc::F_SETFL, flags & !libc::O_NONBLOCK);
                *flag = true;
            }
        }
    }
    changed
}

fn restore_stdio_nonblocking(changed: [bool; STDIO_FDS]) {
    for (fd, was_changed) in changed.into_iter().enumerate() {
        if was_changed {
            // SAFETY: FFI: fcntl on a descriptor number is always safe to call.
            unsafe {
                let flags = libc::fcntl(fd as i32, libc::F_GETFL);
                if flags >= 0 {
                    libc::fcntl(fd as i32, libc::F_SETFL, flags | libc::O_NONBLOCK);
                }
            }
        }
    }
}

/// Whether `fd` is ready in `dir` right now, without waiting.
fn poll_ready(fd: i32, dir: Dir) -> bool {
    let events = match dir {
        Dir::Read => libc::POLLIN,
        Dir::Write => libc::POLLOUT,
    };
    let mut pfd = libc::pollfd {
        fd,
        events,
        revents: 0,
    };
    // SAFETY: FFI: one valid pollfd, zero timeout.
    let n = unsafe { retry_eintr(|| libc::poll(&mut pfd, 1, 0)) };
    n > 0
}

/// Whether `fd` has been closed. A waiter woken `Ready` checks this before it
/// registers again, so it does not re-arm an fd that is being torn down.
fn fd_is_closed(fd: i32) -> bool {
    // SAFETY: FFI: F_GETFD only queries the descriptor.
    let ret = unsafe { libc::fcntl(fd, libc::F_GETFD) };
    ret == -1 && std::io::Error::last_os_error().raw_os_error() == Some(libc::EBADF)
}

/// One I/O attempt from a thread.
fn virtual_thread_io<'gc>(
    ctx: MutationContext<'gc>,
    cx: &crate::runtime::sched::scheduler::SchedCtx,
    fd: i32,
    dir: Dir,
    mut attempt: impl FnMut() -> Result<isize, i32>,
) -> (MutationContext<'gc>, Result<isize, i32>) {
    ensure_nonblocking(fd);
    let would_block = |errno: i32| errno == libc::EAGAIN || errno == libc::EWOULDBLOCK;
    let mut ctx = ctx;
    loop {
        match attempt() {
            Err(libc::EINTR) => continue,
            Err(errno) if would_block(errno) => {}
            done => return (ctx, done),
        }
        let (next, woken) = cx.await_fd_in(ctx, fd, dir, || match attempt() {
            Err(errno) if would_block(errno) => None,
            done => Some(done),
        });
        ctx = next;
        let woken = woken.unwrap_or_else(|killed| killed.unwind());
        match woken {
            // The reactor cannot watch this fd (kqueue refuses a tty such as
            // macOS's /dev/tty). Like Go, wait for it on a blocking thread
            // and retry.
            Err(errno) if errno != libc::EBADF => {
                let (next, waited) = blocking::run(ctx, cx, move || match dir {
                    Dir::Read => blocking_poll_read(fd),
                    Dir::Write => blocking_poll_write(fd),
                });
                ctx = next;
                waited.unwrap_or_else(|killed| killed.unwind());
            }
            Err(errno) => return (ctx, Err(errno)),
            Ok(Woken::Ready) if fd_is_closed(fd) => return (ctx, Err(libc::EBADF)),
            Ok(Woken::Done(Err(libc::EINTR))) | Ok(Woken::Ready) => continue,
            Ok(Woken::Done(done)) => return (ctx, done),
            Ok(Woken::Closed) => return (ctx, Err(libc::EBADF)),
            Ok(Woken::Interrupted) => {
                crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
            }
        }
    }
}

/// `%await-readable` and `%await-writable`. Outside a thread this blocks
/// the calling thread, as the other blocking primitives do.
/// Returns the refreshed context and `Err(errno)` when the fd is closed while
/// waiting or cannot be polled.
fn await_native<'gc>(
    ctx: MutationContext<'gc>,
    fd: i32,
    dir: Dir,
) -> (MutationContext<'gc>, Result<(), i32>) {
    let Some(cx) = current() else {
        ctx.call_in_native(|| {
            if !poll_ready(fd, dir) {
                match dir {
                    Dir::Read => blocking_poll_read(fd),
                    Dir::Write => blocking_poll_write(fd),
                }
            }
        });
        return (ctx, Ok(()));
    };
    let mut ctx = ctx;
    loop {
        let (next, woken) = cx.await_fd_in(ctx, fd, dir, || poll_ready(fd, dir).then_some(()));
        ctx = next;
        let woken = woken.unwrap_or_else(|killed| killed.unwind());
        match woken {
            Ok(Woken::Done(())) => return (ctx, Ok(())),
            Ok(Woken::Ready) if fd_is_closed(fd) => return (ctx, Err(libc::EBADF)),
            Ok(Woken::Ready) => continue,
            Ok(Woken::Interrupted) => {
                crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
            }
            Ok(Woken::Closed) => return (ctx, Err(libc::EBADF)),
            Err(errno) => return (ctx, Err(errno)),
        }
    }
}

/// Poll `fd` for the directions set in `read` and `write`, until `deadline`
/// (`None` waits forever). Returns `Some(code)` on an event: 0 when the fd can
/// be used, 2 when the fd is not open. `None` when the deadline passes first.
fn poll_fd_until(
    fd: i32,
    read: bool,
    write: bool,
    deadline: Option<std::time::Instant>,
) -> Option<i32> {
    let mut events = 0;
    if read {
        events |= libc::POLLIN;
    }
    if write {
        events |= libc::POLLOUT;
    }
    loop {
        let timeout_ms = match deadline {
            None => -1,
            Some(d) => {
                let left_us = d
                    .saturating_duration_since(std::time::Instant::now())
                    .as_micros();
                i32::try_from(left_us.div_ceil(1000)).unwrap_or(i32::MAX)
            }
        };
        let mut pfd = libc::pollfd {
            fd,
            events,
            revents: 0,
        };
        // SAFETY: FFI: one valid pollfd.
        let n = unsafe { libc::poll(&mut pfd, 1, timeout_ms) };
        if n < 0 {
            if std::io::Error::last_os_error().raw_os_error() == Some(libc::EINTR) {
                continue;
            }
            // Cannot be polled, so the caller retries the operation.
            return Some(0);
        }
        if n == 0 {
            return None;
        }
        return Some(if pfd.revents & libc::POLLNVAL != 0 {
            2
        } else {
            0
        });
    }
}

fn await_fd_timed<'gc>(
    ctx: MutationContext<'gc>,
    fd: i32,
    read: bool,
    write: bool,
    deadline: Option<std::time::Instant>,
) -> (MutationContext<'gc>, i32) {
    let Some(cx) = current() else {
        let code = ctx.call_in_native(|| poll_fd_until(fd, read, write, deadline).unwrap_or(1));
        return (ctx, code);
    };
    let mut ctx = ctx;
    loop {
        let (next, woken) = cx.await_fd_dirs_in(ctx, fd, read, write, deadline, || {
            poll_fd_until(fd, read, write, Some(std::time::Instant::now()))
        });
        ctx = next;
        let woken = woken.unwrap_or_else(|killed| killed.unwind());
        match woken {
            // Registration failed: EBADF means the fd is not open, and any
            // other errno means it cannot be polled, which counts as ready.
            Err(errno) => return (ctx, if errno == libc::EBADF { 2 } else { 0 }),
            Ok(Woken::Done(code)) => return (ctx, code),
            Ok(Woken::Closed) => return (ctx, 2),
            Ok(Woken::Ready) if fd_is_closed(fd) => return (ctx, 2),
            Ok(Woken::Ready) if deadline.is_some_and(|d| std::time::Instant::now() >= d) => {
                return (ctx, 1);
            }
            Ok(Woken::Ready) => continue,
            Ok(Woken::Interrupted) => {
                crate::runtime::vm::interrupts::deliver_pending_interrupts(ctx);
            }
        }
    }
}

/// Wait on a child process
fn wait_child(pid: i32) -> std::io::Result<i32> {
    let mut status: i32 = 0;
    // SAFETY: FFI: waitpid writes one int through a valid pointer.
    let ret = unsafe { libc::waitpid(pid, &mut status as *mut i32, 0) };
    if ret == -1 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(if libc::WIFEXITED(status) {
        libc::WEXITSTATUS(status)
    } else if libc::WIFSIGNALED(status) {
        -libc::WTERMSIG(status)
    } else {
        -1
    })
}

fn wait_for_events(
    poller: &polling::Poller,
    timeout: Option<std::time::Duration>,
) -> Result<Vec<(usize, i32)>, i32> {
    let mut events = polling::Events::new();
    match poller.wait(&mut events, timeout) {
        Ok(_) => Ok(events
            .iter()
            .map(|event| (event.key, event_to_flags(event)))
            .collect()),
        Err(err) => Err(err.raw_os_error().unwrap_or(i32::MAX)),
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
