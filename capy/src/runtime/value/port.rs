use crate::{
    Trace,
    prelude::{ScmHeader, Tagged, TypeCode8},
};

#[repr(C)]
pub struct Socket {
    pub(crate) header: ScmHeader,
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

unsafe impl Tagged for Socket {
    const TYPE_NAME: &'static str = "socket";
    const TC8: super::TypeCode8 = TypeCode8::SOCKET;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]

pub enum SocketMode {
    None,
    Server,
    Client,
}
