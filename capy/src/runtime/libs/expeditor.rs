use crate::prelude::*;
use crossterm::cursor;
use crossterm::terminal;
use std::mem::MaybeUninit;
static mut TERM_OUT_MBS: libc::mbstate_t = unsafe { MaybeUninit::zeroed().assume_init() };
static mut TERM_IN_MBS: libc::mbstate_t = unsafe { MaybeUninit::zeroed().assume_init() };

fn term_out_mbs() -> *mut libc::mbstate_t {
    &raw mut TERM_OUT_MBS
}

fn term_in_mbs() -> *mut libc::mbstate_t {
    &raw mut TERM_IN_MBS
}

#[scheme(path = capy::expeditor)]
pub mod expeditor {
    use std::{io::Write, os::fd::AsRawFd};

    use crate::runtime::{YieldReason, vm::io::IoOperation};

    #[scheme(name = "init-term")]
    pub fn init_term(_input: i32, _output: i32) -> i32 {
        todo!()
    }

    #[scheme(name = "$ee-read-char")]
    pub fn read_char(blockp: bool) -> Value<'gc> {
        let fd = std::io::stdin();
        let raw_fd = fd.as_raw_fd();
        let mut n;
        let mut buf = [0u8; 1];
        let mut wch: libc::wchar_t = 0;
        let mut sz = 0usize;
        loop {
            if !blockp && ctx.nest_level() == 1 {
                unsafe {
                    libc::fcntl(
                        raw_fd,
                        libc::F_SETFL,
                        libc::fcntl(raw_fd, libc::F_GETFL) | libc::O_NONBLOCK,
                    );
                    n = libc::read(raw_fd, buf.as_mut_ptr() as *mut libc::c_void, 1);
                    libc::fcntl(
                        raw_fd,
                        libc::F_SETFL,
                        libc::fcntl(raw_fd, libc::F_GETFL) & !libc::O_NONBLOCK,
                    );
                    if n < 0 && errno::errno().0 == libc::EWOULDBLOCK {
                        if !blockp {
                            return nctx.return_(Value::new(false));
                        }

                        // yield and poll on FD for readiness.
                        // Once ready runtime will re-invoke `read-char`.
                        return nctx.yield_(YieldReason::PollRead(raw_fd));
                    }
                }
            } else {
                n = unsafe { libc::read(raw_fd, buf.as_mut_ptr() as *mut libc::c_void, 1) };
            }

            if n == 1 {
                if buf[0] == 0 {
                    return nctx.return_(Value::new('\0'));
                } else {
                    let sz =
                        unsafe { super::mbrtowc(&mut wch, buf.as_ptr().cast(), 1, term_out_mbs()) };
                    if sz == 1 {
                        return nctx.return_(Value::new(char::from_u32(wch as u32).unwrap()));
                    }
                }
            } else {
                sz = 0;
            }

            if n < 0 && errno::errno().0 == libc::EINTR || n == 1 && sz == (-2isize) as usize {
                continue;
            } else {
                break;
            }
        }

        if n == 0 {
            return nctx.return_(Value::eof());
        }

        unsafe {
            TERM_OUT_MBS = MaybeUninit::zeroed().assume_init();
        }

        let err = errno::errno();

        nctx.raise_io_error(
            std::io::Error::from_raw_os_error(err.0),
            IoOperation::Read,
            "read-char",
            "error reading from console",
            ctx.intern("<stdin>"),
        )
    }

    #[scheme(name = "$ee-write-char")]
    pub fn write_char(ch: char) -> Value<'gc> {
        let fd = std::io::stdout();
        let raw_fd = fd.as_raw_fd();
        let mut buf = [0u8; 4];
        let wch = ch as libc::wchar_t;
        let sz =
            unsafe { super::wcrtomb(buf.as_mut_ptr() as *mut libc::c_char, wch, term_in_mbs()) };

        if sz == (-1isize) as usize {
            unsafe { libc::putchar(b'?' as i32) };
        } else {
            unsafe {
                libc::write(raw_fd, buf.as_ptr() as *const libc::c_void, sz);
            }
        }

        nctx.return_(Value::undefined())
    }

    #[scheme(name = "get-screen-size")]
    pub fn get_screen_size() -> (u16, u16) {
        let winsize = crossterm::terminal::size().unwrap_or((80, 24));

        nctx.return_(winsize)
    }

    #[scheme(name = "raw-mode")]
    pub fn raw_mode() -> Value<'gc> {
        match crossterm::terminal::enable_raw_mode() {
            Ok(_) => (),
            Err(e) => {
                return nctx.raise_io_error(
                    e,
                    IoOperation::Open,
                    "raw-mode",
                    "error enabling raw mode",
                    ctx.intern("<stdout>"),
                );
            }
        }

        nctx.return_(Value::undefined())
    }

    #[scheme(name = "no-raw-mode")]
    pub fn no_raw_mode() -> Value<'gc> {
        match crossterm::terminal::disable_raw_mode() {
            Ok(_) => (),
            Err(e) => {
                return nctx.raise_io_error(
                    e,
                    IoOperation::Open,
                    "no-raw-mode",
                    "error disabling raw mode",
                    ctx.intern("<stdout>"),
                );
            }
        }

        nctx.return_(Value::undefined())
    }

    #[scheme(name = "postoutput")]
    pub fn postoutput() -> Value<'gc> {
        unsafe {
            let mut new_termios: libc::termios = MaybeUninit::zeroed().assume_init();
            libc::tcgetattr(libc::STDIN_FILENO, &mut new_termios);
            new_termios.c_oflag |= libc::OPOST;
            libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, &new_termios);
        }
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "no-postoutput")]
    pub fn no_postoutput() -> Value<'gc> {
        unsafe {
            let mut new_termios: libc::termios = MaybeUninit::zeroed().assume_init();
            libc::tcgetattr(libc::STDIN_FILENO, &mut new_termios);
            new_termios.c_oflag &= !libc::OPOST;
            libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, &new_termios);
        }
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "signal")]
    pub fn signal() -> i32 {
        unsafe {
            let mut new_termios: libc::termios = MaybeUninit::zeroed().assume_init();
            libc::tcgetattr(libc::STDIN_FILENO, &mut new_termios);
            new_termios.c_lflag |= libc::ISIG;
            libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, &new_termios);
        }
        nctx.return_(0)
    }

    #[scheme(name = "no-signal")]
    pub fn no_signal() -> i32 {
        unsafe {
            let mut new_termios: libc::termios = MaybeUninit::zeroed().assume_init();
            libc::tcgetattr(libc::STDIN_FILENO, &mut new_termios);
            new_termios.c_lflag &= !libc::ISIG;
            libc::tcsetattr(libc::STDIN_FILENO, libc::TCSANOW, &new_termios);
        }
        nctx.return_(0)
    }

    #[scheme(name = "enter-am-mode")]
    pub fn enter_am_mode() -> Value<'gc> {
        // enter auto-margin mode
        let mut stdout = std::io::stdout();
        crossterm::queue!(stdout, cursor::MoveLeft(1), cursor::MoveRight(1)).unwrap();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "exit-am-mode")]
    pub fn exit_am_mode() -> Value<'gc> {
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "pause")]
    pub fn pause() -> Value<'gc> {
        let mut stdout = std::io::stdout();
        stdout.flush().unwrap();
        unsafe {
            libc::kill(0, libc::SIGTSTP);
        }
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "move-cursor-up")]
    pub fn up(lines: u16) -> Value<'gc> {
        let mut stdout = std::io::stdout();
        crossterm::queue!(stdout, cursor::MoveUp(lines)).unwrap();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "move-cursor-down")]
    pub fn down(lines: u16) -> Value<'gc> {
        let mut stdout = std::io::stdout();
        crossterm::queue!(stdout, cursor::MoveDown(lines)).unwrap();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "$move-cursor-right")]
    pub fn right(columns: u16) -> Value<'gc> {
        let mut stdout = std::io::stdout();
        crossterm::queue!(stdout, cursor::MoveRight(columns)).unwrap();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "$move-cursor-left")]
    pub fn left(columns: u16) -> Value<'gc> {
        let mut stdout = std::io::stdout();
        crossterm::queue!(stdout, cursor::MoveLeft(columns)).unwrap();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "clear-eol")]
    pub fn clear_eol() -> Value<'gc> {
        let mut stdout = std::io::stdout();
        crossterm::queue!(stdout, terminal::Clear(terminal::ClearType::UntilNewLine)).unwrap();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "clear-eos")]
    pub fn clear_eos() -> Value<'gc> {
        let mut stdout = std::io::stdout();
        crossterm::queue!(stdout, terminal::Clear(terminal::ClearType::FromCursorDown)).unwrap();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "$clear-screen")]
    pub fn clear_screen() -> Value<'gc> {
        let mut stdout = std::io::stdout();
        crossterm::queue!(stdout, terminal::Clear(terminal::ClearType::All)).unwrap();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "bell")]
    pub fn bell() -> Value<'gc> {
        let mut stdout = std::io::stdout();
        write!(stdout, "\x07").unwrap();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "$carriage-return")]
    pub fn carriage_return() -> Value<'gc> {
        let mut stdout = std::io::stdout();
        crossterm::queue!(stdout, cursor::MoveToColumn(0)).unwrap();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "line-feed")]
    pub fn line_feed() -> Value<'gc> {
        let mut stdout = std::io::stdout();
        write!(stdout, "\n").unwrap();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }

    #[scheme(name = "ee-flush")]
    pub fn ee_flush() -> Value<'gc> {
        let mut stdout = std::io::stdout();
        stdout.flush().unwrap();
        nctx.return_(Value::undefined())
    }
}

unsafe extern "C" {
    unsafe fn mbrtowc(
        pwc: *mut libc::wchar_t,
        s: *const libc::c_char,
        n: usize,
        ps: *mut libc::mbstate_t,
    ) -> usize;

    unsafe fn wcrtomb(s: *mut libc::c_char, wc: libc::wchar_t, ps: *mut libc::mbstate_t) -> usize;
}

pub fn init_expeditor<'gc>(ctx: Context<'gc>) {
    expeditor::register(ctx);
}
