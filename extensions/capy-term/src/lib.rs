use capy::{prelude::*, runtime::vm::VMResult};
use libc::{
    STDOUT_FILENO, TCSANOW, TIOCGWINSZ, cfmakeraw, ioctl, tcgetattr, tcsetattr, termios as Termios,
    winsize,
};
use std::os::fd::{IntoRawFd, RawFd};

fn tty_fd() -> (libc::c_int, bool) {
    unsafe {
        if libc::isatty(libc::STDIN_FILENO) == 1 {
            (libc::STDIN_FILENO, false)
        } else {
            let file = std::fs::File::open("/dev/tty").expect("Failed to open /dev/tty");
            (file.into_raw_fd(), true)
        }
    }
}

fn wrap_with_result(result: i32) -> std::io::Result<()> {
    if result == -1 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn get_terminal_attr(fd: RawFd) -> std::io::Result<Termios> {
    unsafe {
        let mut termios = std::mem::MaybeUninit::zeroed();
        wrap_with_result(tcgetattr(fd, termios.as_mut_ptr()))?;
        Ok(termios.assume_init())
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn capy_register_extension<'gc>(ctx: &Context<'gc>) -> VMResult<'gc> {
    //term_ops::register(*ctx);
    VMResult::Ok(Value::new(true))
}
