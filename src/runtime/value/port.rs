
fn io_close(fd: i32) {
    unsafe {
        libc::close(fd);
    }
}

fn io_read(fd: i32, dest: &mut [u8]) -> usize {
    unsafe {
        libc::read(fd, dest.as_mut_ptr() as *mut libc::c_void, dest.len()) as usize
    }
}

fn io_write(fd: i32, src: &[u8]) -> usize {
    unsafe {
        libc::write(fd, src.as_ptr() as *const libc::c_void, src.len()) as usize
    }
}

fn io_pread(fd: i32, dest: &mut [u8], offset: i64) -> usize {
    unsafe {
        libc::pread(
            fd,
            dest.as_mut_ptr() as *mut libc::c_void,
            dest.len(),
            offset as libc::off_t,
        ) as usize
    }
}

fn io_pwrite(fd: i32, src: &[u8], offset: i64) -> usize {
    unsafe {
        libc::pwrite(
            fd,
            src.as_ptr() as *const libc::c_void,
            src.len(),
            offset as libc::off_t,
        ) as usize
    }
}

fn io_lseek64(fd: i32, offset: i64, whence: i32) -> i64 {
    unsafe { libc::lseek(fd, offset as libc::off_t, whence) as i64 }
}

fn io_mkstemp() -> i32 {
    unsafe { libc::mkstemp(std::ptr::null_mut()) }
}

fn io_open(path: &str, flags: i32) -> i32 {
    let c_path = std::ffi::CString::new(path).unwrap();
    unsafe { libc::open(c_path.as_ptr(), flags) }
}

