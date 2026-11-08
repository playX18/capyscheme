//! Physical and currently-available memory, used to size the starting heap.

/// Machine memory at one point in time, in bytes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct SystemMemory {
    pub(crate) total: usize,
    /// Memory that could be given to this process now without swapping.
    pub(crate) available: usize,
}

/// Returns the current memory figures, or `None` when the platform is not
/// supported or the figures cannot be read.
pub(crate) fn system_memory() -> Option<SystemMemory> {
    imp::system_memory()
}

#[cfg(target_os = "macos")]
mod imp {
    use super::SystemMemory;
    use std::ffi::{CString, c_void};

    /// Reads a sysctl integer.  Values are read into a zeroed `u64`, which
    /// covers both 32-bit counters and 64-bit sizes on little-endian hosts.
    fn sysctl_u64(name: &str) -> Option<u64> {
        let name = CString::new(name).ok()?;
        let mut value: u64 = 0;
        let mut len = std::mem::size_of::<u64>();
        // SAFETY: `name` is NUL-terminated, and `value`/`len` describe a buffer
        // large enough for any integer sysctl we query.
        let rc = unsafe {
            libc::sysctlbyname(
                name.as_ptr(),
                (&mut value as *mut u64).cast::<c_void>(),
                &mut len,
                std::ptr::null_mut(),
                0,
            )
        };
        (rc == 0).then_some(value)
    }

    pub(super) fn system_memory() -> Option<SystemMemory> {
        // SAFETY: `sysconf` has no preconditions.
        let page_size = usize::try_from(unsafe { libc::sysconf(libc::_SC_PAGESIZE) }).ok()?;
        let total = usize::try_from(sysctl_u64("hw.memsize")?).ok()?;
        // Free pages plus file-backed pages the kernel can reclaim on demand.
        let free = sysctl_u64("vm.page_free_count")?;
        let reclaimable = sysctl_u64("vm.page_pageable_external_count").unwrap_or(0);
        let available = usize::try_from(free.saturating_add(reclaimable)).ok()? * page_size;
        Some(SystemMemory {
            total,
            available: available.min(total),
        })
    }
}

#[cfg(target_os = "linux")]
mod imp {
    use super::SystemMemory;

    pub(super) fn system_memory() -> Option<SystemMemory> {
        let text = std::fs::read_to_string("/proc/meminfo").ok()?;
        let field = |key: &str| {
            text.lines()
                .find_map(|line| line.strip_prefix(key))
                .and_then(|rest| rest.split_whitespace().next())
                .and_then(|kb| kb.parse::<usize>().ok())
                .map(|kb| kb * 1024)
        };
        Some(SystemMemory {
            total: field("MemTotal:")?,
            available: field("MemAvailable:")?,
        })
    }
}

#[cfg(not(any(target_os = "macos", target_os = "linux")))]
mod imp {
    use super::SystemMemory;

    pub(super) fn system_memory() -> Option<SystemMemory> {
        None
    }
}
