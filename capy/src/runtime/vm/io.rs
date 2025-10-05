use std::ffi::CString;

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
