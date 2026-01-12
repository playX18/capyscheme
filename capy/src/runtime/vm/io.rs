use crate::prelude::port::Socket;
use crate::prelude::*;
use crate::runtime::{AfterBlockingOperationCallback, BlockingOperationWithReturn};
use crate::static_symbols;
use mmtk::util::options::PlanSelector;
use rustix::fd::AsRawFd;
use std::ffi::CString;
static_symbols!(
    SYM_INPUT = "input"
    SYM_OUTPUT = "output"
    SYM_ERROR = "error"
    SYM_BINARY = "binary"
    SYM_TEXTUAL = "textual"
);

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
    use std::{ffi::CStr, ptr::null_mut};

    use mmtk::AllocationSemantics;

    use crate::{
        CAN_PIN_OBJECTS,
        prelude::port::{Socket, SocketMode},
        runtime::PollOperation,
    };

    #[scheme(name = "usleep")]
    pub fn usleep(microseconds: u64) -> bool {
        std::thread::sleep(std::time::Duration::from_micros(microseconds as u64));
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
                for entry in entries {
                    if let Ok(entry) = entry {
                        let name = entry.file_name().to_string_lossy().into_owned();
                        ls = Value::cons(ctx, Str::new(*ctx, &name, true).into(), ls);
                    }
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
                    nctx.return_(Str::new(*ctx, &path.to_string(), true))
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
        nctx.return_(rustix::fs::access(&path.to_string(), rustix::fs::Access::READ_OK).is_ok())
    }

    #[scheme(name = "file-writable?")]
    pub fn file_writable(path: Gc<'gc, Str<'gc>>) -> bool {
        nctx.return_(rustix::fs::access(&path.to_string(), rustix::fs::Access::WRITE_OK).is_ok())
    }

    #[scheme(name = "file-executable?")]
    pub fn file_executable(path: Gc<'gc, Str<'gc>>) -> bool {
        nctx.return_(rustix::fs::access(&path.to_string(), rustix::fs::Access::EXEC_OK).is_ok())
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
        match rustix::fs::chmod(&path.to_string(), mode) {
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
                nctx.return_(s_str.len() == 1 && s_str.chars().next().unwrap() == sep)
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
            let cpath = std::ffi::CString::new(path).unwrap();
            nctx.return_(libc::open(cpath.as_ptr(), oflag, pmode))
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

        if filename.len() == 0 {
            return nctx.return_(-1);
        }

        unsafe {
            let cpath = CString::new(filename.to_string()).unwrap();
            // make fd non-blocking. syscalls are not exposed publicly thus its fine to
            // just make our own FDs non-blocking.
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

        unsafe {
            let ptr = buf.as_slice_mut_unchecked().as_mut_ptr() as *mut libc::c_void;
            let ret = libc::read(fd, ptr, nbytes);
            if ret == -1
                && (errno::errno().0 == libc::EAGAIN || errno::errno().0 == libc::EWOULDBLOCK)
            {
                return nctx.perform(PollOperation::Read(fd));
            }
            nctx.return_(ret)
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

        unsafe {
            let ptr = buf.as_slice().as_ptr().wrapping_add(offset) as *const libc::c_void;
            let ret = libc::write(fd, ptr, nbytes);
            if ret == -1
                && (errno::errno().0 == libc::EAGAIN || errno::errno().0 == libc::EWOULDBLOCK)
            {
                return nctx.perform(PollOperation::Write(fd));
            }
            nctx.return_(ret)
        }
    }

    #[scheme(name = "syscall:lseek")]
    pub fn syscall_lseek(fd: i32, offset: i64, whence: i32) -> i64 {
        unsafe {
            let whence = match whence {
                0 => libc::SEEK_SET,
                1 => libc::SEEK_CUR,
                2 => libc::SEEK_END,
                _ => return nctx.return_(-1),
            };
            let ret = libc::lseek(fd, offset, whence);
            if ret == -1
                && (errno::errno().0 == libc::EAGAIN || errno::errno().0 == libc::EWOULDBLOCK)
            {
                return nctx.perform(PollOperation::Write(fd));
            }
            nctx.return_(ret)
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
        let cpath = CString::new(filename.to_string()).unwrap();
        unsafe {
            let ret = libc::unlink(cpath.as_ptr());
            nctx.return_(ret)
        }
    }

    #[scheme(name = "syscall:rename")]
    pub fn syscall_rename(old_filename: Gc<'gc, Str<'gc>>, new_filename: Gc<'gc, Str<'gc>>) -> i32 {
        let cold = CString::new(old_filename.to_string()).unwrap();
        let cnew = CString::new(new_filename.to_string()).unwrap();
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
            let cpath = CString::new(path).unwrap();
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

        let cpath = CString::new(filename.to_string()).unwrap();
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

        unsafe {
            let ptr = buf.as_slice_mut_unchecked().as_mut_ptr() as *mut libc::c_void;
            let ret = libc::recv(fd, ptr, nbytes, flags);
            if ret == -1
                && (errno::errno().0 == libc::EAGAIN || errno::errno().0 == libc::EWOULDBLOCK)
            {
                return nctx.perform(PollOperation::Read(fd));
            }
            nctx.return_(ret)
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

        unsafe {
            let ptr = buf.as_slice().as_ptr() as *const libc::c_void;
            let ret = libc::send(fd, ptr, nbytes, flags);
            if ret == -1
                && (errno::errno().0 == libc::EAGAIN || errno::errno().0 == libc::EWOULDBLOCK)
            {
                return nctx.perform(PollOperation::Write(fd));
            }
            nctx.return_(ret)
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
        let ccommand = CString::new(command.to_string()).unwrap();
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
                CString::new(path.to_string()).unwrap().as_ptr(),
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

        let ifd = child.stdin.as_ref().unwrap().as_raw_fd();
        let ofd = child.stdout.as_ref().unwrap().as_raw_fd();
        let efd = child.stderr.as_ref().unwrap().as_raw_fd();
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
        let poller = Gc::new(
            *ctx,
            Poller {
                header: ScmHeader::with_type_bits(TypeCode8::POLLER.bits() as _),
                inner,
            },
        );

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
    pub fn poller_wait(poller: Gc<'gc, Poller>, timeout: Option<i32>) -> () {
        let k = make_closure_poller_wait_k(ctx, [nctx.retk, nctx.reth]);
        let operation = PollerOperation { poller, timeout };
        nctx.perform_returning_to(k.into(), operation)
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

    #[scheme(continuation, name = "${ poller-wait/k }")]
    #[allow(non_snake_case)]
    fn poller_wait_k(succ: bool, arg: Value<'gc>) -> Value<'gc> {
        // [0] = <native-proc>
        // [1] = retk
        // [2] = reth
        let retk = nctx.rator().downcast::<Closure>().free.downcast::<Vector>()[1].get();
        let reth = nctx.rator().downcast::<Closure>().free.downcast::<Vector>()[2].get();

        nctx.retk = retk;
        nctx.reth = reth;

        if succ {
            nctx.return_(arg)
        } else {
            nctx.raise_error("poller-wait", "failed to wait for events", &[arg])
        }
    }

}

pub fn init_io<'gc>(ctx: Context<'gc>) {
    io_ops::register(ctx);

    let module = ctx.module("capy").unwrap();
    let host_features_tab = HashTable::new(*ctx, HashTableType::Eq, 64, 0.80);
    module.define(ctx, ctx.intern("*host-features*"), host_features_tab.into());

    macro_rules! cconst {
        ($name: ident) => {
            let name = stringify!($name);
            let sym = ctx.intern(name);
            host_features_tab.put(ctx, sym, Value::new(libc::$name));
        };
    }

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
    header: ScmHeader,
    inner: polling::Poller,
}

unsafe impl Tagged for Poller {
    const TC8: TypeCode8 = TypeCode8::POLLER;
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

pub struct PollerOperation<'gc> {
    poller: Gc<'gc, Poller>,
    timeout: Option<i32>,
}

unsafe impl<'gc> Trace for PollerOperation<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        visitor.trace(&mut self.poller);
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> BlockingOperationWithReturn<'gc> for PollerOperation<'gc> {
    fn prepare(&self, _ctx: Context<'gc>) -> Box<dyn FnOnce() -> AfterBlockingOperationCallback> {
        let timeout = self
            .timeout
            .map(|t| std::time::Duration::from_millis(t as u64));
        let ptr = Gc::as_ptr(self.poller);

        Box::new(move || unsafe {
            let poller = &*ptr;
            let mut events = polling::Events::new();
            let res = poller.inner.wait(&mut events, timeout);

            Box::new(move |ctx, args: &mut Vec<Value<'_>>| {
                // now return to Scheme code by building correct args to our `retk`.

                args.push(Value::new(res.is_ok()));
                match res {
                    Ok(_) => {
                        let mut ls = Value::null();
                        for event in events.iter() {
                            let key = event.key;
                            let flags = event_to_flags(event);
                            ls = Value::cons(
                                ctx,
                                Value::cons(ctx, key.into_value(ctx), flags.into_value(ctx)),
                                ls,
                            );
                        }
                        args.push(ls);
                    }

                    Err(err) => args.push(err.raw_os_error().unwrap_or(i32::MAX).into()),
                }
            })
        })
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

pub struct SendOperation<'gc> {
    sock: Gc<'gc, Socket>,
    bv: Gc<'gc, ByteVector>,
    flags: i32,
}

unsafe impl<'gc> Trace for SendOperation<'gc> {
    unsafe fn trace(&mut self, visitor: &mut Visitor) {
        let gc = crate::rsgc::GarbageCollector::get();
        let opts = gc.mmtk.get_options();
        match *opts.plan {
            PlanSelector::MarkSweep
            | PlanSelector::StickyImmix
            | PlanSelector::Immix
            | PlanSelector::ConcurrentImmix => {
                visitor.pin_root(self.sock.to_object_reference());
                visitor.pin_root(self.bv.to_object_reference());
            }

            _ => {
                // TODO: This can move the pointers and invalidate them. We solve this
                // by allocating on non-moving heap for these types when using GenImmix/GenCopy/Semispace.
                visitor.trace(&mut self.sock);
                visitor.trace(&mut self.bv);
            }
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> BlockingOperationWithReturn<'gc> for SendOperation<'gc> {
    fn prepare(&self, _ctx: Context<'gc>) -> Box<dyn FnOnce() -> AfterBlockingOperationCallback> {
        let sock = Gc::as_ptr(self.sock);
        let bv = Gc::as_ptr(self.bv);
        let flags = self.flags;

        Box::new(move || unsafe {
            let sock = &*sock;
            let bv = &*bv;
            let res = libc::send(
                sock.fd,
                bv.as_ptr() as _,
                bv.len() as _,
                flags as libc::c_int,
            );

            Box::new(move |ctx, args: &mut Vec<Value<'_>>| args.push(res.into_value(ctx)))
        })
    }
}
