use crate::rsgc::sync::thread::Thread;
use mmtk::util::Address;
use std::sync::{
    Once,
    atomic::{AtomicBool, Ordering},
};

static INSTALL_YIELDPOINT_SIGNAL_HANDLER: Once = Once::new();
static PREVIOUS_SIGSEGV_SET: AtomicBool = AtomicBool::new(false);
static PREVIOUS_SIGBUS_SET: AtomicBool = AtomicBool::new(false);
static mut PREVIOUS_SIGSEGV: std::mem::MaybeUninit<libc::sigaction> =
    std::mem::MaybeUninit::uninit();
static mut PREVIOUS_SIGBUS: std::mem::MaybeUninit<libc::sigaction> =
    std::mem::MaybeUninit::uninit();

pub(crate) fn install_yieldpoint_signal_handler() {
    INSTALL_YIELDPOINT_SIGNAL_HANDLER.call_once(|| unsafe {
        install_signal_handler_for(libc::SIGSEGV);
        install_signal_handler_for(libc::SIGBUS);
    });
}

unsafe fn install_signal_handler_for(signum: libc::c_int) {
    unsafe {
        let mut action: libc::sigaction = std::mem::zeroed();
        action.sa_sigaction = yieldpoint_signal_handler as *const () as usize;
        action.sa_flags = libc::SA_SIGINFO | libc::SA_ONSTACK;
        libc::sigemptyset(&mut action.sa_mask);

        let previous = previous_action_slot(signum);
        if libc::sigaction(signum, &action, previous.as_mut_ptr()) != 0 {
            panic!("failed to install yieldpoint signal handler");
        }

        previous_action_set(signum).store(true, Ordering::Release);
    }
}

extern "C" fn yieldpoint_signal_handler(
    signum: libc::c_int,
    info: *mut libc::siginfo_t,
    context: *mut libc::c_void,
) {
    let Some(thread) = Thread::try_current() else {
        unsafe { delegate_signal(signum) };
        return;
    };

    let fault_addr = unsafe { Address::from_ptr((*info).si_addr()) };
    if !thread.is_yieldpoint_page_fault(fault_addr) {
        unsafe { delegate_signal(signum) };
        return;
    }

    let Some(thread_context) =
        (unsafe { crate::rsgc::sync::safepoint::PlatformThreadContext::from_ucontext(context) })
    else {
        unsafe { delegate_signal(signum) };
        return;
    };

    // This deliberately runs non-async-signal-safe GC rendezvous code from the
    // fault handler, matching the requested page-fault yieldpoint semantics.
    Thread::handle_yieldpoint_fault(thread_context);
}

unsafe fn delegate_signal(signum: libc::c_int) {
    unsafe {
        if previous_action_set(signum).load(Ordering::Acquire) {
            let previous = previous_action_slot(signum).assume_init_ref();
            libc::sigaction(signum, previous, std::ptr::null_mut());
        }
        libc::raise(signum);
    }
}

unsafe fn previous_action_slot(
    signum: libc::c_int,
) -> &'static mut std::mem::MaybeUninit<libc::sigaction> {
    unsafe {
        if signum == libc::SIGSEGV {
            &mut *std::ptr::addr_of_mut!(PREVIOUS_SIGSEGV)
        } else {
            &mut *std::ptr::addr_of_mut!(PREVIOUS_SIGBUS)
        }
    }
}

fn previous_action_set(signum: libc::c_int) -> &'static AtomicBool {
    if signum == libc::SIGSEGV {
        &PREVIOUS_SIGSEGV_SET
    } else {
        &PREVIOUS_SIGBUS_SET
    }
}
