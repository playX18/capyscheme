//! Helpers for reading GC header metadata from value payloads.

use crate::rsgc::mmtk::util::Address;
use crate::rsgc::object::{GCObject, HeapObjectHeader};

/// Returns the heap header for a payload pointer.
pub fn heap_header<T>(payload: &T) -> &HeapObjectHeader {
    GCObject::from_address(Address::from_ref(payload)).header()
}

/// Returns the type bits stored in the payload object's heap header.
pub fn payload_type_bits<T>(payload: &T) -> u16 {
    heap_header(payload).type_bits()
}

/// Returns the heap type-info identifier stored in the payload object's header.
pub fn payload_info_id<T>(payload: &T) -> u16 {
    heap_header(payload).info_id()
}
