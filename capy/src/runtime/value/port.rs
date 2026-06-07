//! Port-adjacent socket value types.

use crate::{Trace, prelude::ClassTagged, rsgc::object::builtin_class_ids};

#[repr(C)]
/// Runtime socket object.
pub struct Socket {
    pub(crate) addr: libc::sockaddr_storage,
    pub(crate) fd: i32,
    pub(crate) mode: SocketMode,
    pub(crate) family: i32,
    pub(crate) socktype: i32,
    pub(crate) protocol: i32,
    pub(crate) addrlen: i32,
}

unsafe impl Trace for Socket {
    unsafe fn trace(&mut self, visitor: &mut crate::Visitor) {
        let _ = visitor;
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut crate::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl ClassTagged for Socket {
    const CLASS_IDS: &'static [u32] = &[builtin_class_ids::SOCKET];
    const TYPE_NAME: &'static str = "socket";
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]

/// Socket role tracked by the runtime.
pub enum SocketMode {
    None,
    Server,
    Client,
}
