use mmtk::util::Address;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

pub(crate) const MAX_CONTEXT_REGISTER_WORDS: usize = 32;

#[derive(Debug)]
#[repr(C)]
pub(crate) struct YieldpointPage {
    pub(crate) address: AtomicUsize,
    size: usize,
    protected: AtomicBool,
}

impl YieldpointPage {
    pub(crate) fn new() -> Self {
        let size = page_size();
        let ptr = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | map_anonymous_flag(),
                -1,
                0,
            )
        };

        if ptr == libc::MAP_FAILED {
            panic!("failed to allocate yieldpoint page");
        }

        Self {
            address: AtomicUsize::new(ptr as usize),
            size,
            protected: AtomicBool::new(false),
        }
    }

    pub(crate) fn contains(&self, addr: Address) -> bool {
        let start = self.address.load(Ordering::Relaxed);
        let end = start.saturating_add(self.size);
        let addr = addr.as_usize();
        addr >= start && addr < end
    }

    pub(crate) fn protect(&self) {
        if self
            .protected
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Relaxed)
            .is_ok()
        {
            let result = unsafe {
                libc::mprotect(
                    self.address.load(Ordering::Relaxed) as *mut libc::c_void,
                    self.size,
                    libc::PROT_NONE,
                )
            };
            if result != 0 {
                self.protected.store(false, Ordering::Release);
                panic!("failed to protect yieldpoint page");
            }
        }
    }

    pub(crate) fn unprotect(&self) {
        if self
            .protected
            .compare_exchange(true, false, Ordering::AcqRel, Ordering::Relaxed)
            .is_ok()
        {
            let result = unsafe {
                libc::mprotect(
                    self.address.load(Ordering::Relaxed) as *mut libc::c_void,
                    self.size,
                    libc::PROT_READ | libc::PROT_WRITE,
                )
            };
            if result != 0 {
                self.protected.store(true, Ordering::Release);
                panic!("failed to unprotect yieldpoint page");
            }
        }
    }
}

impl Drop for YieldpointPage {
    fn drop(&mut self) {
        let addr = self.address.load(Ordering::Relaxed);
        if addr != 0 {
            unsafe {
                libc::munmap(addr as *mut libc::c_void, self.size);
            }
        }
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub(crate) struct PlatformThreadContext {
    stack_pointer: usize,
    register_words: [usize; MAX_CONTEXT_REGISTER_WORDS],
    register_word_count: usize,
}

impl PlatformThreadContext {
    pub(crate) const fn empty() -> Self {
        Self {
            stack_pointer: 0,
            register_words: [0; MAX_CONTEXT_REGISTER_WORDS],
            register_word_count: 0,
        }
    }

    pub(crate) fn from_stack_pointer(stack_pointer: Address) -> Self {
        Self {
            stack_pointer: stack_pointer.as_usize(),
            register_words: [0; MAX_CONTEXT_REGISTER_WORDS],
            register_word_count: 0,
        }
    }

    pub(crate) fn stack_pointer(self) -> Address {
        unsafe { Address::from_usize(self.stack_pointer) }
    }

    pub(crate) fn register_words(&self) -> &[usize] {
        &self.register_words[..self.register_word_count]
    }

    pub(crate) unsafe fn from_ucontext(context: *mut libc::c_void) -> Option<Self> {
        unsafe { platform_thread_context_from_ucontext(context) }
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
unsafe fn platform_thread_context_from_ucontext(
    context: *mut libc::c_void,
) -> Option<PlatformThreadContext> {
    unsafe {
        let context = (context as *const libc::ucontext_t).as_ref()?;
        let gregs = &context.uc_mcontext.gregs;
        Some(context_from_gregs(
            gregs[libc::REG_RSP as usize] as usize,
            gregs.iter().map(|value| *value as usize),
        ))
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86"))]
unsafe fn platform_thread_context_from_ucontext(
    context: *mut libc::c_void,
) -> Option<PlatformThreadContext> {
    unsafe {
        let context = (context as *const libc::ucontext_t).as_ref()?;
        let gregs = &context.uc_mcontext.gregs;
        Some(context_from_gregs(
            gregs[libc::REG_ESP as usize] as usize,
            gregs.iter().map(|value| *value as usize),
        ))
    }
}

#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
unsafe fn platform_thread_context_from_ucontext(
    context: *mut libc::c_void,
) -> Option<PlatformThreadContext> {
    unsafe {
        let context = (context as *const libc::ucontext_t).as_ref()?;
        let mut words = context
            .uc_mcontext
            .regs
            .iter()
            .copied()
            .map(|value| value as usize);
        Some(context_from_gregs(
            context.uc_mcontext.sp as usize,
            (&mut words).chain(std::iter::once(context.uc_mcontext.pc as usize)),
        ))
    }
}

#[cfg(all(target_os = "macos", target_arch = "x86_64"))]
unsafe fn platform_thread_context_from_ucontext(
    context: *mut libc::c_void,
) -> Option<PlatformThreadContext> {
    unsafe {
        let context = (context as *const libc::ucontext_t).as_ref()?;
        let state = &(*context.uc_mcontext).__ss;
        Some(context_from_gregs(
            state.__rsp as usize,
            [
                state.__rax as usize,
                state.__rbx as usize,
                state.__rcx as usize,
                state.__rdx as usize,
                state.__rdi as usize,
                state.__rsi as usize,
                state.__rbp as usize,
                state.__rsp as usize,
                state.__r8 as usize,
                state.__r9 as usize,
                state.__r10 as usize,
                state.__r11 as usize,
                state.__r12 as usize,
                state.__r13 as usize,
                state.__r14 as usize,
                state.__r15 as usize,
                state.__rip as usize,
            ],
        ))
    }
}

#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
unsafe fn platform_thread_context_from_ucontext(
    context: *mut libc::c_void,
) -> Option<PlatformThreadContext> {
    unsafe {
        let context = (context as *const libc::ucontext_t).as_ref()?;
        let state = &(*context.uc_mcontext).__ss;
        Some(context_from_gregs(
            state.__sp as usize,
            state
                .__x
                .iter()
                .copied()
                .map(|value| value as usize)
                .chain([state.__fp as usize, state.__lr as usize, state.__pc as usize]),
        ))
    }
}

#[cfg(all(
    any(
        target_os = "freebsd",
        target_os = "openbsd",
        target_os = "netbsd",
        target_os = "dragonfly"
    ),
    target_arch = "x86_64"
))]
unsafe fn platform_thread_context_from_ucontext(
    context: *mut libc::c_void,
) -> Option<PlatformThreadContext> {
    unsafe {
        let context = (context as *const libc::ucontext_t).as_ref()?;
        let mc = &context.uc_mcontext;
        Some(context_from_gregs(mc.mc_rsp as usize, [mc.mc_rsp as usize]))
    }
}

#[cfg(all(
    any(
        target_os = "freebsd",
        target_os = "openbsd",
        target_os = "netbsd",
        target_os = "dragonfly"
    ),
    target_arch = "x86"
))]
unsafe fn platform_thread_context_from_ucontext(
    context: *mut libc::c_void,
) -> Option<PlatformThreadContext> {
    unsafe {
        let context = (context as *const libc::ucontext_t).as_ref()?;
        let mc = &context.uc_mcontext;
        Some(context_from_gregs(mc.mc_esp as usize, [mc.mc_esp as usize]))
    }
}

#[cfg(all(
    any(
        target_os = "freebsd",
        target_os = "openbsd",
        target_os = "netbsd",
        target_os = "dragonfly"
    ),
    target_arch = "aarch64"
))]
unsafe fn platform_thread_context_from_ucontext(
    context: *mut libc::c_void,
) -> Option<PlatformThreadContext> {
    unsafe {
        let context = (context as *const libc::ucontext_t).as_ref()?;
        let mc = &context.uc_mcontext;
        Some(context_from_gregs(mc.mc_sp as usize, [mc.mc_sp as usize]))
    }
}

#[cfg(not(any(
    all(target_os = "linux", target_arch = "x86_64"),
    all(target_os = "linux", target_arch = "x86"),
    all(target_os = "linux", target_arch = "aarch64"),
    all(target_os = "macos", target_arch = "x86_64"),
    all(target_os = "macos", target_arch = "aarch64"),
    all(
        any(
            target_os = "freebsd",
            target_os = "openbsd",
            target_os = "netbsd",
            target_os = "dragonfly"
        ),
        any(target_arch = "x86", target_arch = "x86_64", target_arch = "aarch64")
    )
)))]
unsafe fn platform_thread_context_from_ucontext(
    _context: *mut libc::c_void,
) -> Option<PlatformThreadContext> {
    None
}

fn context_from_gregs(
    stack_pointer: usize,
    registers: impl IntoIterator<Item = usize>,
) -> PlatformThreadContext {
    let mut context = PlatformThreadContext::from_stack_pointer(unsafe {
        Address::from_usize(stack_pointer)
    });
    for (index, value) in registers
        .into_iter()
        .take(MAX_CONTEXT_REGISTER_WORDS)
        .enumerate()
    {
        context.register_words[index] = value;
        context.register_word_count = index + 1;
    }
    context
}

fn page_size() -> usize {
    let size = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };
    if size <= 0 { 4096 } else { size as usize }
}

#[cfg(any(target_os = "macos", target_os = "ios"))]
const fn map_anonymous_flag() -> libc::c_int {
    libc::MAP_ANON
}

#[cfg(not(any(target_os = "macos", target_os = "ios")))]
const fn map_anonymous_flag() -> libc::c_int {
    libc::MAP_ANONYMOUS
}
