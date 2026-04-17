use crate::rsgc::mmtk::util::Address;
use crate::rsgc::object::{GCObject, HeapObjectHeader};

pub fn heap_header<T>(payload: &T) -> &HeapObjectHeader {
    GCObject::from_address(Address::from_ref(payload)).header()
}

pub fn payload_type_bits<T>(payload: &T) -> u16 {
    heap_header(payload).type_bits()
}

pub fn payload_info_id<T>(payload: &T) -> u16 {
    heap_header(payload).info_id()
}
