//! Helpers for reading GC header metadata from value payloads.

use crate::heap::mmtk::util::Address;
use crate::heap::object::{ClassId, GcObject, HeapObjectHeader};

/// Returns the heap header for a payload pointer.
pub fn heap_header<T>(payload: &T) -> &HeapObjectHeader {
    GcObject::from_address(Address::from_ref(payload)).header()
}

/// Returns the class identifier stored in the payload object's heap header.
pub fn payload_class_id<T>(payload: &T) -> ClassId {
    heap_header(payload).class_id()
}
