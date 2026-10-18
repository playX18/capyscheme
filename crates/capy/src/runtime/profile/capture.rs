//! Capture for profiler using `SIGPROF`

use std::sync::atomic::{AtomicU64, Ordering::Relaxed};

use super::ARMED;
use super::maps;
use crate::bytecode::def;
use crate::heap::sync::thread::Thread;
use crate::runtime::interp::markers;
use crate::runtime::interp::stack;
use crate::runtime::sabi::{self, Ra};

/// Ring slots per thread
pub const RING_SAMPLES: usize = 512;
const RING_MASK: u64 = (RING_SAMPLES - 1) as u64;
/// Frames per sample, running frame included.
pub const MAX_FRAMES: usize = 64;

/// Set when the walk stopped early
pub const SAMPLE_TRUNCATED: u32 = 0x01;

/// One walked frame: the return address, the `fp[-1]` resume slot, and
/// the `fp[1]` value bits.
#[derive(Clone, Copy, Default)]
pub struct FrameSample {
    pub ra: u64,
    pub resume: u64,
    pub closure: u64,
}

/// One captured stack, as the handler fills it on its own stack.
pub struct Sample {
    pub sfp: u64,
    pub pcreg: u64,
    pub rip: u64,
    pub nframes: u32,
    pub flags: u32,
    pub frames: [FrameSample; MAX_FRAMES],
}

impl Sample {
    pub(crate) fn empty() -> Self {
        Self {
            sfp: 0,
            pcreg: 0,
            rip: 0,
            nframes: 0,
            flags: 0,
            frames: [FrameSample::default(); MAX_FRAMES],
        }
    }
}

/// One ring slot
pub struct SampleSlot {
    seq: AtomicU64,
    sfp: u64,
    pcreg: u64,
    rip: u64,
    nframes: u32,
    flags: u32,
    frames: [FrameSample; MAX_FRAMES],
}

impl SampleSlot {
    const fn new() -> Self {
        Self {
            seq: AtomicU64::new(0),
            sfp: 0,
            pcreg: 0,
            rip: 0,
            nframes: 0,
            flags: 0,
            frames: [FrameSample {
                ra: 0,
                resume: 0,
                closure: 0,
            }; MAX_FRAMES],
        }
    }
}

/// What [`SampleSlot::read_sealed`] found.
pub enum SealOutcome {
    /// A complete sample of the expected generation.
    Sample(Sample),
    /// The slot is being written, the caller retries or drops.
    Torn,
    /// The slot holds a different (newer) sample, ours was evicted.
    Evicted,
}

/// The per-thread sample ring: single-writer (the sampled thread's
/// handler), single logical reader (the aggregator, serialized per ring
/// with unbind drains through [`ProfileRing::drain_lock`]).
pub struct ProfileRing {
    head: AtomicU64,
    tail: AtomicU64,
    slots: Box<[SampleSlot; RING_SAMPLES]>,
    /// Serializes drains of this ring (aggregator vs thread unbind).
    /// The handler never touches it.
    pub drain_lock: parking_lot::Mutex<()>,
}

impl ProfileRing {
    pub fn new() -> Box<Self> {
        let mut slots = Box::<[SampleSlot; RING_SAMPLES]>::new_uninit();
        let base = slots.as_mut_ptr() as *mut SampleSlot;
        for index in 0..RING_SAMPLES {
            // SAFETY: in-bounds, each slot written exactly once.
            unsafe { base.add(index).write(SampleSlot::new()) };
        }
        // SAFETY: every slot was just written.
        let slots = unsafe { slots.assume_init() };
        Box::new(Self {
            head: AtomicU64::new(0),
            tail: AtomicU64::new(0),
            slots,
            drain_lock: parking_lot::Mutex::new(()),
        })
    }

    /// Record `sample`, evicting the oldest on a full ring. Runs on the
    /// owning thread only. Returns whether a sample was evicted.
    pub fn push(&self, sample: &Sample) -> bool {
        let tail = self.tail.load(Relaxed);
        let head = self.head.load(Relaxed);
        let evicted = head.wrapping_sub(tail) >= RING_SAMPLES as u64;
        if evicted {
            // The aggregator may be reading this slot, its seqlock check
            // rejects the torn generation.
            self.tail.fetch_add(1, Relaxed);
        }
        let slot = &self.slots[(head & RING_MASK) as usize] as *const SampleSlot as *mut SampleSlot;

        // SAFETY: the owning thread's handler is the only writer of
        // this slot, readers validate the seqlock generations around
        // their copies.
        unsafe {
            (*slot)
                .seq
                .store(head.wrapping_mul(2).wrapping_add(1), Relaxed);
            std::ptr::addr_of_mut!((*slot).sfp).write(sample.sfp);
            std::ptr::addr_of_mut!((*slot).pcreg).write(sample.pcreg);
            std::ptr::addr_of_mut!((*slot).rip).write(sample.rip);
            std::ptr::addr_of_mut!((*slot).nframes).write(sample.nframes);
            std::ptr::addr_of_mut!((*slot).flags).write(sample.flags);
            std::ptr::addr_of_mut!((*slot).frames).write(sample.frames);
            (*slot)
                .seq
                .store(head.wrapping_mul(2).wrapping_add(2), Relaxed);
        }
        self.head.fetch_add(1, Relaxed);
        evicted
    }

    /// Copy the sample at `tail` when its seal is intact.
    fn read_sealed(&self, tail: u64) -> SealOutcome {
        let slot = &self.slots[(tail & RING_MASK) as usize];
        let expected = tail.wrapping_mul(2).wrapping_add(2);
        let mut spins = 0;
        loop {
            let observed = slot.seq.load(Relaxed);
            if observed == expected {
                let mut sample = Sample::empty();
                // SAFETY: the slot outlives the ring, which outlives the
                // aggregator's borrow, torn values are rejected by the
                // generation check below.
                unsafe {
                    sample.sfp = std::ptr::addr_of!((*slot).sfp).read();
                    sample.pcreg = std::ptr::addr_of!((*slot).pcreg).read();
                    sample.rip = std::ptr::addr_of!((*slot).rip).read();
                    sample.nframes = std::ptr::addr_of!((*slot).nframes).read();
                    sample.flags = std::ptr::addr_of!((*slot).flags).read();
                    sample.frames = std::ptr::addr_of!((*slot).frames).read();
                }
                if slot.seq.load(Relaxed) == observed {
                    return SealOutcome::Sample(sample);
                }
                continue;
            }
            if observed == expected.wrapping_sub(1) {
                // Our sample, mid-write: spin briefly, then give up.
                spins += 1;
                if spins > 100 {
                    return SealOutcome::Torn;
                }
                std::hint::spin_loop();
                continue;
            }
            // Any other generation: a newer sample reused the slot.
            return SealOutcome::Evicted;
        }
    }

    /// Drain newly recorded samples into `out`. The caller holds
    /// [`ProfileRing::drain_lock`], progress is a compare-exchange so a
    /// racing handler evict cannot skip a sample.
    pub fn drain_into(&self, out: &mut Vec<Sample>) {
        loop {
            let tail = self.tail.load(Relaxed);
            let head = self.head.load(Relaxed);
            if tail >= head {
                return;
            }
            match self.read_sealed(tail) {
                SealOutcome::Sample(sample) => out.push(sample),
                SealOutcome::Torn => {
                    TORN.fetch_add(1, Relaxed);
                }
                SealOutcome::Evicted => {}
            }
            if self
                .tail
                .compare_exchange(tail, tail.wrapping_add(1), Relaxed, Relaxed)
                .is_err()
            {
                // A handler evict advanced past us, re-read positions.
                continue;
            }
        }
    }
}

// SAFETY: the ring is written by its owning thread's handler and read
// by the aggregator through atomics and the drain lock.
unsafe impl Send for ProfileRing {}
unsafe impl Sync for ProfileRing {}

/// Capture counters, all handler-safe `Relaxed` atomics, snapshotted
/// into the output header.
pub static SAMPLES: AtomicU64 = AtomicU64::new(0);
pub static DROPPED_UNBOUND: AtomicU64 = AtomicU64::new(0);
pub static DROPPED_NOT_IN_SCHEME: AtomicU64 = AtomicU64::new(0);
pub static DROPPED_RINGLESS: AtomicU64 = AtomicU64::new(0);
pub static DROPPED_REGS: AtomicU64 = AtomicU64::new(0);
pub static DROPPED_BAD_FP: AtomicU64 = AtomicU64::new(0);
pub static DROPPED_EMPTY: AtomicU64 = AtomicU64::new(0);
pub static TRUNCATED: AtomicU64 = AtomicU64::new(0);
pub static OVERWRITTEN: AtomicU64 = AtomicU64::new(0);
pub static TORN: AtomicU64 = AtomicU64::new(0);

/// Machine registers captured from `ucontext`: the Scheme frame pointer
/// (`SFP`: `rbp`/`x20`), the interpreter pc register when live (`PC`:
/// `r8`/`x23`), and the interrupted instruction pointer.
pub struct InterruptedRegs {
    pub sfp: u64,
    pub pcreg: u64,
    pub rip: u64,
}

/// Read the sampled thread's registers from its `ucontext`. `None`
/// when the platform exposes no layout
pub fn interrupted_regs(uctx: *mut libc::c_void) -> Option<InterruptedRegs> {
    if uctx.is_null() {
        return None;
    }
    // SAFETY: `uctx` is the kernel-built context for `SIGPROF` with
    // `SA_SIGINFO`, each branch reads its own platform layout only.
    unsafe { interrupted_regs_platform(uctx) }
}

#[cfg(all(target_vendor = "apple", target_arch = "aarch64"))]
unsafe fn interrupted_regs_platform(uctx: *mut libc::c_void) -> Option<InterruptedRegs> {
    unsafe {
        let context = &*(uctx as *const libc::ucontext_t);
        let machine = context.uc_mcontext.as_ref()?;
        let state = &machine.__ss;
        Some(InterruptedRegs {
            sfp: state.__x[20] as u64,
            pcreg: state.__x[23] as u64,
            rip: state.__pc as u64,
        })
    }
}

#[cfg(all(target_vendor = "apple", target_arch = "x86_64"))]
unsafe fn interrupted_regs_platform(uctx: *mut libc::c_void) -> Option<InterruptedRegs> {
    unsafe {
        let context = &*(uctx as *const libc::ucontext_t);
        let machine = context.uc_mcontext.as_ref()?;
        let state = &machine.__ss;
        Some(InterruptedRegs {
            sfp: state.__rbp as u64,
            pcreg: state.__r8 as u64,
            rip: state.__rip as u64,
        })
    }
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
unsafe fn interrupted_regs_platform(uctx: *mut libc::c_void) -> Option<InterruptedRegs> {
    unsafe {
        let context = &*(uctx as *const libc::ucontext_t);
        let regs = &context.uc_mcontext.gregs;
        Some(InterruptedRegs {
            sfp: regs[libc::REG_RBP as usize] as u64,
            pcreg: regs[libc::REG_R8 as usize] as u64,
            rip: regs[libc::REG_RIP as usize] as u64,
        })
    }
}

#[cfg(all(target_os = "linux", target_arch = "aarch64"))]
unsafe fn interrupted_regs_platform(uctx: *mut libc::c_void) -> Option<InterruptedRegs> {
    unsafe {
        let context = &*(uctx as *const libc::ucontext_t);
        let machine = &context.uc_mcontext;
        Some(InterruptedRegs {
            sfp: machine.regs[20] as u64,
            pcreg: machine.regs[23] as u64,
            rip: machine.pc as u64,
        })
    }
}

#[cfg(not(any(
    all(target_vendor = "apple", target_arch = "aarch64"),
    all(target_vendor = "apple", target_arch = "x86_64"),
    all(target_os = "linux", target_arch = "x86_64"),
    all(target_os = "linux", target_arch = "aarch64"),
)))]
unsafe fn interrupted_regs_platform(_uctx: *mut libc::c_void) -> Option<InterruptedRegs> {
    None
}

/// Whether this platform can capture registers
pub fn platform_supported() -> bool {
    cfg!(any(
        all(target_vendor = "apple", target_arch = "aarch64"),
        all(target_vendor = "apple", target_arch = "x86_64"),
        all(target_os = "linux", target_arch = "x86_64"),
        all(target_os = "linux", target_arch = "aarch64"),
    ))
}

/// Walk the live stack from `sfp`, recording up to `frames.len()`
/// frames.
///
/// # Safety
///
/// The caller guarantees `sfp` passed the same segment bounds (the
/// walk re-checks per step) and `frames` is a live caller buffer,
/// headers and return points validate themselves.
pub unsafe fn walk_stack(
    sfp: u64,
    stack_lo: u64,
    stack_hi: u64,
    frames: &mut [FrameSample],
) -> (usize, bool) {
    // SAFETY: every slot, header, and
    // return point read below is mapped, each step re-checks.
    unsafe {
        let in_segment = |fp: u64| {
            stack_lo <= fp && fp < stack_hi && fp & 0x7 == 0 && fp.wrapping_add(16) <= stack_hi
        };
        if !in_segment(sfp) {
            return (0, true);
        }
        let (interp_return, scheme_exit, underflow, raise_ret, subr_ret) = (
            markers::interpreter_return() as u64,
            markers::scheme_exit() as u64,
            markers::underflow() as u64,
            markers::raise_ret() as u64,
            markers::subr_ret() as u64,
        );
        let mut fp = sfp as *mut u64;
        let mut count = 0;
        while count < frames.len() {
            let fp_addr = fp as u64;
            if !in_segment(fp_addr) {
                return (count, true);
            }
            let ra = fp.read();
            let resume = fp.sub(1).read();
            let closure = fp.add(1).read();
            frames[count] = FrameSample {
                ra,
                resume,
                closure,
            };
            count += 1;
            if ra == scheme_exit || ra == underflow {
                return (count, false);
            }
            let size: usize = if ra == interp_return {
                match decode_return_point_size(resume as stack::Ip) {
                    Some(size) => size,
                    None => return (count, true),
                }
            } else if ra == raise_ret {
                // Below a `raise_ret` frame sits its size, not an ip.
                let size = resume as usize;
                if size == 0 || size > 1 << 30 {
                    return (count, true);
                }
                size / size_of::<u64>()
            } else if ra == subr_ret {
                // A native continuation's frame: return address and
                // closure, `SUBR_RET_FRAME_SIZE` by construction on
                // both markers (the static slot and the
                // generated stub emit the same constant).
                markers::SUBR_RET_FRAME_SIZE as usize / size_of::<u64>()
            } else {
                let Some((base, _)) = maps::handler_code_span(ra) else {
                    return (count, true);
                };
                if ra.saturating_sub(8) < base {
                    return (count, true);
                }
                // SAFETY: `[ra - 8, ra)` is inside the span (bound
                // above), so the discriminator word is mapped.
                let compact = sabi::is_entry_compact(ra as Ra);
                let header_len = if compact {
                    sabi::SIZE_RP_COMPACT_HEADER as u64
                } else {
                    sabi::SIZE_RP_HEADER as u64
                };
                if ra.saturating_sub(header_len) < base {
                    return (count, true);
                }
                // SAFETY: the selected header lies inside the span
                // (bound above).
                let frame_bytes = sabi::entry_frame_size(ra as Ra);
                if frame_bytes == sabi::INTERPRETER_FRAME_SIZE || frame_bytes <= 0 {
                    return (count, true);
                }
                frame_bytes as usize / size_of::<u64>()
            };
            if size == 0 {
                return (count, true);
            }
            fp = fp.sub(size);

            if fp as u64 >= fp_addr {
                return (count, true);
            }
        }
        (count, true)
    }
}

/// Frame size of the return point at `return-point`, or `None` when it does
/// not decode to one.
unsafe fn decode_return_point_size(retpt: stack::Ip) -> Option<usize> {
    unsafe {
        if retpt.is_null() {
            return None;
        }

        if maps::handler_bytecode_span(retpt as u64).is_none() {
            return None;
        }
        let (width, opcode, operands) = def::decode_header(retpt);
        match opcode {
            def::OpcodeId::OpReturnPoint => {
                let (op, _) = def::OpReturnPoint::decode_from_ptr(operands, width);
                Some(op.frame.0 as usize)
            }
            def::OpcodeId::OpReturnPointForward => {
                let (op, _) = def::OpReturnPointForward::decode_from_ptr(operands, width);
                Some(op.frame.0 as usize)
            }
            _ => None,
        }
    }
}

/// The `SIGPROF` handler
pub(crate) unsafe extern "C" fn sigprof_handler(
    _sig: libc::c_int,
    _info: *mut libc::siginfo_t,
    uctx: *mut libc::c_void,
) {
    // SAFETY: signal context, every access below is bounds-checked,
    // validated, or a lock-free atomic.
    unsafe {
        let Some(thread) = Thread::try_current_thread() else {
            DROPPED_UNBOUND.fetch_add(1, Relaxed);
            return;
        };
        if !ARMED.load(Relaxed) {
            return;
        }

        let tc = &*thread.tc_ptr();
        if tc.stack.activation.get().is_null() {
            DROPPED_NOT_IN_SCHEME.fetch_add(1, Relaxed);
            return;
        }
        let ring = thread.prof_ring.load(Relaxed);
        if ring.is_null() {
            DROPPED_RINGLESS.fetch_add(1, Relaxed);
            return;
        }
        let Some(regs) = interrupted_regs(uctx) else {
            DROPPED_REGS.fetch_add(1, Relaxed);
            return;
        };
        let stack_lo = tc.stack.scheme_stack.get() as u64;
        let stack_hi = stack_lo
            .wrapping_add(tc.stack.scheme_stack_size.get() as u64 * size_of::<u64>() as u64);

        let mut sfp = regs.sfp;
        if !(stack_lo..stack_hi).contains(&sfp) {
            sfp = tc.stack.fp.get() as u64;
            if !(stack_lo..stack_hi).contains(&sfp) {
                DROPPED_BAD_FP.fetch_add(1, Relaxed);
                return;
            }
        }
        let ring = &*ring;
        let mut sample = Sample::empty();
        sample.sfp = sfp;
        sample.pcreg = regs.pcreg;
        sample.rip = regs.rip;
        let (nframes, truncated) = walk_stack(sfp, stack_lo, stack_hi, &mut sample.frames);
        if nframes == 0 {
            DROPPED_EMPTY.fetch_add(1, Relaxed);
            return;
        }
        sample.nframes = nframes as u32;
        if truncated {
            sample.flags |= SAMPLE_TRUNCATED;
            TRUNCATED.fetch_add(1, Relaxed);
        }
        if ring.push(&sample) {
            OVERWRITTEN.fetch_add(1, Relaxed);
        }
        SAMPLES.fetch_add(1, Relaxed);
    }
}

/// Block `SIGPROF` on the calling thread: worker and aggregator
/// threads never sample, so they opt out at spawn.
pub fn block_sigprof_for_worker() {
    unsafe {
        let mut set: libc::sigset_t = std::mem::zeroed();
        libc::sigemptyset(&mut set);
        libc::sigaddset(&mut set, libc::SIGPROF);
        libc::pthread_sigmask(libc::SIG_BLOCK, &set, std::ptr::null_mut());
    }
}
