use std::ffi::CString;

use crate::{native_fn, runtime::prelude::*, static_symbols};

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

native_fn!(
    register_io_fns:

    pub ("dirname") fn dirname<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> Gc<'gc, Str<'gc>> {
        let p = std::path::PathBuf::from(path.to_string());
        let dir = p.parent().unwrap_or_else(|| std::path::Path::new(""));
        let s = dir.to_string_lossy();
        let ctx = nctx.ctx;
        nctx.return_(Str::new(&ctx, &s, true))
    }

    pub ("getenv") fn getenv<'gc>(nctx, var: Gc<'gc, Str<'gc>>) -> Value<'gc> {
        match std::env::var(var.to_string()) {
            Ok(s) => {
                let ctx = nctx.ctx;
                nctx.return_(Str::new(&ctx, &s, true).into())
            }
            Err(_) => {
                nctx.return_(Value::new(false))
            }
        }
    }

    pub ("directory-list") fn directory_list<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> Value<'gc> {
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
                    path.into()
                )
            }
        }
    }

    pub ("current-directory") fn current_directory<'gc>(nctx) -> Gc<'gc, Str<'gc>> {
        match std::env::current_dir() {
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
                    Value::new(false)
                )
            }
        }
    }

    pub ("create-directory") fn create_directory<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::create_dir(path.to_string()) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "create-directory",
                    &error,
                    path.into()
                )
            }
        }
    }

    pub ("delete-file") fn delete_file<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::remove_file(path.to_string()) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "delete-file",
                    &error,
                    path.into()
                )
            }
        }
    }

    pub ("file-size-in-bytes") fn file_size_in_bytes<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> i64 {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => {
                nctx.return_(metadata.len() as i64)
            }
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-size-in-bytes",
                    &error,
                    path.into()
                )
            }
        }
    }

    pub ("file-regular?") fn file_regular<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => {
                nctx.return_(metadata.is_file())
            }
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-regular?",
                    &error,
                    path.into()
                )
            }
        }
    }

    pub ("file-directory?") fn file_directory<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => {
                nctx.return_(metadata.is_dir())
            }
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-directory?",
                    &error,
                    path.into()
                )
            }
        }
    }

    pub ("file-symbolic-link?") fn file_symbolic_link<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::symlink_metadata(path.to_string()) {
            Ok(metadata) => {
                nctx.return_(metadata.file_type().is_symlink())
            }
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-symbolic-link?",
                    &error,
                    path.into()
                )
            }
        }
    }

    pub ("file-readable?") fn file_readable<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        nctx.return_(rustix::fs::access(&path.to_string(), rustix::fs::Access::READ_OK).is_ok())
    }

    pub ("file-writable?") fn file_writable<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        nctx.return_(rustix::fs::access(&path.to_string(), rustix::fs::Access::WRITE_OK).is_ok())
    }

    pub ("file-executable?") fn file_executable<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        nctx.return_(rustix::fs::access(&path.to_string(), rustix::fs::Access::EXEC_OK).is_ok())
    }

    pub ("change-file-mode") fn change_file_mode<'gc>(nctx, path: Gc<'gc, Str<'gc>>, mode: i32) -> bool {
        let Some(mode) = rustix::fs::Mode::from_bits(mode as _) else {
            return nctx.wrong_argument_violation(
                "change-file-mode",
                "invalid mode bits",
                Some(mode.into()),
                Some(0),
                2,
                &[path.into(), mode.into()]
            );
        };
        match rustix::fs::chmod(&path.to_string(), mode) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                nctx.raise_io_error(
                    std::io::Error::new(err.kind(), "failed to change mode"),
                    IoOperation::Open,
                    "change-file-mode",
                    &err.to_string(),
                    path.into()
                )
            }
        }
    }

    pub ("rename-file") fn rename_file<'gc>(nctx, old_path: Gc<'gc, Str<'gc>>, new_path: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::rename(old_path.to_string(), new_path.to_string()) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_filesystem_error(
                    "rename-file",
                    &error,
                    err,
                    old_path.into(),
                    new_path.into()
                )
            }
        }
    }

    pub ("create-symbolic-link") fn create_symbolic_link<'gc>(nctx, original: Gc<'gc, Str<'gc>>, link: Gc<'gc, Str<'gc>>) -> bool {
        match std::os::unix::fs::symlink(original.to_string(), link.to_string()) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "create-symbolic-link",
                    &error,
                    link.into()
                )
            }
        }
    }

    pub ("create-hard-link") fn create_hard_link<'gc>(nctx, original: Gc<'gc, Str<'gc>>, link: Gc<'gc, Str<'gc>>) -> bool {
        match std::fs::hard_link(original.to_string(), link.to_string()) {
            Ok(()) => nctx.return_(true),
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "create-hard-link",
                    &error,
                    link.into()
                )
            }
        }
    }

    pub ("file-stat-atime") fn file_stat_atime<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> i64 {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => {
                match metadata.accessed() {
                    Ok(time) => {
                        match time.duration_since(std::time::UNIX_EPOCH) {
                            Ok(dur) => nctx.return_(dur.as_secs() as i64),
                            Err(_) => nctx.return_(-1)
                        }
                    }
                    Err(_) => nctx.return_(-1)
                }
            }
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-stat-atime",
                    &error,
                    path.into()
                )
            }
        }
    }

    pub ("file-stat-mtime") fn file_stat_mtime<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> i64 {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => {
                match metadata.modified() {
                    Ok(time) => {
                        match time.duration_since(std::time::UNIX_EPOCH) {
                            Ok(dur) => nctx.return_(dur.as_secs() as i64),
                            Err(_) => nctx.return_(-1)
                        }
                    }
                    Err(_) => nctx.return_(-1)
                }
            }
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-stat-mtime",
                    &error,
                    path.into()
                )
            }
        }
    }

    pub ("file-stat-ctime") fn file_stat_ctime<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> i64 {
        match std::fs::metadata(path.to_string()) {
            Ok(metadata) => {
                match metadata.created() {
                    Ok(time) => {
                        match time.duration_since(std::time::UNIX_EPOCH) {
                            Ok(dur) => nctx.return_(dur.as_secs() as i64),
                            Err(_) => nctx.return_(-1)
                        }
                    }
                    Err(_) => nctx.return_(-1)
                }
            }
            Err(err) => {
                let error = err.to_string();
                nctx.raise_io_error(
                    err,
                    IoOperation::Open,
                    "file-stat-ctime",
                    &error,
                    path.into()
                )
            }
        }
    }


    pub ("getcwd") fn getcwd<'gc>(nctx) -> Gc<'gc, Str<'gc>> {
        let buf = std::env::current_dir().unwrap_or_default();
        let s = buf.to_string_lossy();
        let ctx = nctx.ctx;
        nctx.return_(Str::new(&ctx, &s, true))
    }

    pub ("file-exists?") fn file_exists<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        let metadata = std::fs::metadata(path.to_string());
        nctx.return_(metadata.is_ok())
    }

    pub ("file-name-separator?") fn file_name_separator<'gc>(nctx, s: Gc<'gc, Str<'gc>>) -> bool {
        let sep = std::path::MAIN_SEPARATOR.to_string();
        nctx.return_(s.to_string() == sep)
    }

    pub ("system-file-name-convention") fn system_filename_conv<'gc>(nctx) -> Gc<'gc, Symbol<'gc>> {
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

    pub ("absolute-path-string?") fn is_absolute_path_string<'gc>(nctx, path: Gc<'gc, Str<'gc>>) -> bool {
        let p = std::path::PathBuf::from(path.to_string());
        nctx.return_(p.is_absolute())
    }


    pub ("io/open") fn io_open<'gc>(
        nctx,
        path: Gc<'gc, Str<'gc>>,
        oflag: i32,
        pmode: i32
    ) -> i32 {
        let path = path.to_string();

        unsafe {
            let cpath = std::ffi::CString::new(path).unwrap();
            nctx.return_(libc::open(cpath.as_ptr(), oflag, pmode))
        }
    }

    pub ("io/read") fn io_read<'gc>(
        nctx,
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        from: usize,
        nbytes: usize
    ) -> isize {
        if from + nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "io/read",
                "buffer too small for read",
                None,
                None,
                4,
                &[fd.into(), buf.into(), from.into_value(ctx), nbytes.into_value(ctx)]
            );
        }

        unsafe {
            let ptr = buf.as_slice_mut_unchecked().as_mut_ptr().add(from) as *mut libc::c_void;
            let ret = libc::read(fd, ptr, nbytes);
            nctx.return_(ret)
        }
    }

    pub ("io/write") fn io_write<'gc>(
        nctx,
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        from: usize,
        nbytes: usize
    ) -> isize {
        if from + nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "io/write",
                "buffer too small for write",
                None,
                None,
                4,
                &[fd.into(), buf.into(), from.into_value(ctx), nbytes.into_value(ctx)]
            );
        }

        unsafe {
            let ptr = buf.as_slice().as_ptr().add(from) as *mut libc::c_void;
            let ret = libc::write(fd, ptr, nbytes);
            nctx.return_(ret)
        }
    }

    pub ("io/pread") fn io_pread<'gc>(
        nctx,
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        from: usize,
        nbytes: usize,
        offset: i64
    ) -> isize {
        if from + nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "io/pread",
                "buffer too small for pread",
                None,
                None,
                5,
                &[fd.into(), buf.into(), from.into_value(ctx), nbytes.into_value(ctx), offset.into_value(ctx)]
            );
        }

        unsafe {
            let ptr = buf.as_slice_mut_unchecked().as_mut_ptr().add(from) as *mut libc::c_void;
            let ret = libc::pread(fd, ptr, nbytes, offset);
            nctx.return_(ret)
        }
    }

    pub ("io/pwrite") fn io_pwrite<'gc>(
        nctx,
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        from: usize,
        nbytes: usize,
        offset: i64
    ) -> isize {
        if from + nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "io/pwrite",
                "buffer too small for pwrite",
                None,
                None,
                5,
                &[fd.into(), buf.into(), from.into_value(ctx), nbytes.into_value(ctx), offset.into_value(ctx)]
            );
        }

        unsafe {
            let ptr = buf.as_slice().as_ptr().add(from) as *const libc::c_void;
            let ret = libc::pwrite(fd, ptr, nbytes, offset);
            nctx.return_(ret)
        }
    }

    pub ("io/lseek") fn io_lseek64<'gc>(
        nctx,
        fd: i32,
        offset: i64,
        whence: i32
    ) -> i64 {
        unsafe {
            let ret = libc::lseek(fd, offset, whence);
            nctx.return_(ret)
        }
    }

    pub ("io/mkstemp") fn io_mkstemp<'gc>(
        nctx,
        template: Gc<'gc, Str<'gc>>
    ) -> i32 {
        let cpath = CString::new(template.to_string()).unwrap();
        unsafe {
            let ret = libc::mkstemp(cpath.as_ptr() as *mut libc::c_char);
            nctx.return_(ret)
        }
    }

    pub ("syscall:open") fn syscall_open<'gc>(
        nctx,
        filename: Gc<'gc, Str<'gc>>,
        flags: i32,
        mode: i32
    ) -> i32 {
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
            let ret = libc::open(cpath.as_ptr(), newflags, mode);
            nctx.return_(ret)
        }
    }

    pub ("syscall:close") fn syscall_close<'gc>(
        nctx,
        fd: i32
    ) -> i32 {
        unsafe {
            let ret = libc::close(fd);
            nctx.return_(ret)
        }
    }

    pub ("syscall:read") fn syscall_read<'gc>(
        nctx,
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        nbytes: usize
    ) -> isize {
        if nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "syscall:read",
                "buffer too small for read",
                None,
                None,
                3,
                &[fd.into(), buf.into(), nbytes.into_value(ctx)]
            );
        }

        unsafe {
            let ptr = buf.as_slice_mut_unchecked().as_mut_ptr() as *mut libc::c_void;
            let ret = libc::read(fd, ptr, nbytes);
            nctx.return_(ret)
        }
    }

    pub ("syscall:write") fn syscall_write<'gc>(
        nctx,
        fd: i32,
        buf: Gc<'gc, ByteVector>,
        nbytes: usize,
        offset: usize
    ) -> isize {
        if nbytes > buf.len() {
            let ctx = nctx.ctx;
            return nctx.wrong_argument_violation(
                "syscall:write",
                "buffer too small for write",
                None,
                None,
                3,
                &[fd.into(), buf.into(), nbytes.into_value(ctx)]
            );
        }

        unsafe {
            let ptr = buf.as_slice().as_ptr().wrapping_add(offset) as *const libc::c_void;
            let ret = libc::write(fd, ptr, nbytes);
            nctx.return_(ret)
        }
    }

    pub ("syscall:lseek") fn syscall_lseek<'gc>(
        nctx,
        fd: i32,
        offset: i64,
        whence: i32
    ) -> i64 {
        unsafe {
            let ret = libc::lseek(fd, offset, whence);
            nctx.return_(ret)
        }
    }

    pub ("syscall:pollinput") fn syscall_pollinput<'gc>(
        nctx,
        fd: i32
    ) -> i32 {
        unsafe {
            let mut fds = libc::pollfd {
                fd,
                events: libc::POLLIN,
                revents: 0,
            };
            let ret = libc::poll(&mut fds as *mut libc::pollfd, 1, 0);
            if ret > 0 && (fds.revents & (libc::POLLIN|libc::POLLHUP|libc::POLLERR)) != 0 {
                nctx.return_(1)
            } else {
                nctx.return_(0)
            }
        }
    }

    pub ("syscall:unlink") fn syscall_unlink<'gc>(
        nctx,
        filename: Gc<'gc, Str<'gc>>
    ) -> i32 {
        let cpath = CString::new(filename.to_string()).unwrap();
        unsafe {
            let ret = libc::unlink(cpath.as_ptr());
            nctx.return_(ret)
        }
    }

    pub ("syscall:rename") fn syscall_rename<'gc>(
        nctx,
        old_filename: Gc<'gc, Str<'gc>>,
        new_filename: Gc<'gc, Str<'gc>>
    ) -> i32 {
        let cold = CString::new(old_filename.to_string()).unwrap();
        let cnew = CString::new(new_filename.to_string()).unwrap();
        unsafe {
            let ret = libc::rename(cold.as_ptr(), cnew.as_ptr());
            nctx.return_(ret)
        }
    }

    pub ("syscall:mtime") fn syscall_mtime<'gc>(
        nctx,
        filename: Gc<'gc, Str<'gc>>,
        vbuf: Gc<'gc, Vector<'gc>>
    ) -> i32 {
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
            wbuf[0].unlock().set((1900 + (*tm).tm_year).into_value(nctx.ctx));
            wbuf[1].unlock().set((1 + (*tm).tm_mon).into_value(nctx.ctx));
            wbuf[2].unlock().set((*tm).tm_mday.into_value(nctx.ctx));
            wbuf[3].unlock().set((*tm).tm_hour.into_value(nctx.ctx));
            wbuf[4].unlock().set((*tm).tm_min.into_value(nctx.ctx));
            wbuf[5].unlock().set((*tm).tm_sec.into_value(nctx.ctx));
            nctx.return_(0)
        }
    }

    pub ("syscall:access") fn syscall_access<'gc>(
        nctx,
        filename: Gc<'gc, Str<'gc>>,
        mode: i32
    ) -> i32 {
        let cpath = CString::new(filename.to_string()).unwrap();
        unsafe {
            let ret = libc::access(cpath.as_ptr(), mode);
            nctx.return_(ret)
        }
    }
);

pub fn init_io<'gc>(ctx: Context<'gc>) {
    register_io_fns(ctx);
}
