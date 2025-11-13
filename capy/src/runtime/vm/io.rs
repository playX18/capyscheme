use crate::prelude::*;
use crate::static_symbols;
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
    use crate::runtime::YieldReason;

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
                nctx.return_(Str::new(&ctx, &s, true))
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
                nctx.return_(Str::new(&ctx, &name_str, true))
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
        nctx.return_(Str::new(&ctx, &s, true))
    }

    #[scheme(name = "getenv")]
    pub fn getenv(var: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::env::var(var.to_string()) {
            Ok(s) => {
                let ctx = nctx.ctx;
                nctx.return_(Str::new(&ctx, &s, true).into())
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
                        ls = Value::cons(ctx, Str::new(&ctx, &name, true).into(), ls);
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
                    nctx.return_(Str::new(&ctx, &path.to_string(), true))
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
                    nctx.return_(Str::new(&ctx, &s, true))
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
        nctx.return_(Str::new(&ctx, &s, true))
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
            let gname = Str::new(&ctx, &newname, true);
            let res = crate::list!(ctx, ret, gname);
            nctx.return_(res)
        }
    }

    #[scheme(name = "syscall:open")]
    pub fn syscall_open(filename: Gc<'gc, Str<'gc>>, flags: i32, mode: i32) -> i32 {
        let mut newflags = 0;

        if flags & 0x01 != 0 {
            newflags |= libc::O_RDONLY;
        }

        if flags & 0x02 != 0 {
            newflags |= libc::O_WRONLY;
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
            if ret == -1
                && (errno::errno().0 == libc::EAGAIN || errno::errno().0 == libc::EWOULDBLOCK)
            {
                return nctx.yield_(YieldReason::PollWrite(fd));
            }
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
                return nctx.yield_(YieldReason::PollRead(fd));
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
                return nctx.yield_(YieldReason::PollWrite(fd));
            }
            nctx.return_(ret)
        }
    }

    #[scheme(name = "syscall:lseek")]
    pub fn syscall_lseek(fd: i32, offset: i64, whence: i32) -> i64 {
        unsafe {
            let ret = libc::lseek(fd, offset, whence);
            if ret == -1
                && (errno::errno().0 == libc::EAGAIN || errno::errno().0 == libc::EWOULDBLOCK)
            {
                return nctx.yield_(YieldReason::PollWrite(fd));
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
            let wbuf = Gc::write(&nctx.ctx, vbuf);
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

    #[scheme(name = "process-environment->alist")]
    pub fn process_environment_to_alist() -> Value<'gc> {
        let ctx = nctx.ctx;
        let mut alist = Value::null();
        for (key, value) in std::env::vars() {
            let key_str = Str::new(&ctx, &key, true);
            let value_str = Str::new(&ctx, &value, true);
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
        nctx.return_((errno.0, Str::new(&ctx, &message, true)))
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
}

pub fn init_io<'gc>(ctx: Context<'gc>) {
    io_ops::register(ctx);
}
