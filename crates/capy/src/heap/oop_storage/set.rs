//! OopStorageSet — registry of strong and weak storages.

use std::sync::OnceLock;

use mmtk::vm::RootsWorkFactory;
use parking_lot::Mutex;

use crate::heap::ObjectSlot;
use crate::runtime::value::Value;

use super::storage::OopStorage;

/// Process-wide registry of [`OopStorage`] instances (HotSpot `OopStorageSet`).
pub struct OopStorageSet {
    strong: Mutex<Vec<&'static OopStorage>>,
    weak: Mutex<Vec<&'static OopStorage>>,
    /// Convenience strong storage for VM / FFI handles.
    pub vm_global: &'static OopStorage,
    /// SNI-style local references for nested Scheme calls.
    pub sni_locals: &'static OopStorage,
    /// SNI-style global references (survive local frame pop).
    pub sni_globals: &'static OopStorage,
}

impl OopStorageSet {
    fn init() -> Self {
        let vm_global = Box::leak(OopStorage::create_strong("VM Global"));
        let sni_locals = Box::leak(OopStorage::create_strong("SNI Locals"));
        let sni_globals = Box::leak(OopStorage::create_strong("SNI Globals"));
        Self {
            strong: Mutex::new(vec![vm_global, sni_locals, sni_globals]),
            weak: Mutex::new(Vec::new()),
            vm_global,
            sni_locals,
            sni_globals,
        }
    }

    pub fn get() -> &'static Self {
        static SET: OnceLock<OopStorageSet> = OnceLock::new();
        SET.get_or_init(Self::init)
    }

    pub fn create_strong(&self, name: &'static str) -> &'static OopStorage {
        let storage = Box::leak(OopStorage::create_strong(name));
        self.strong.lock().push(storage);
        storage
    }

    pub fn create_weak(&self, name: &'static str) -> &'static OopStorage {
        let storage = Box::leak(OopStorage::create_weak(name));
        self.weak.lock().push(storage);
        storage
    }

    pub fn strong_storages(&self) -> Vec<&'static OopStorage> {
        self.strong.lock().clone()
    }

    pub fn weak_storages(&self) -> Vec<&'static OopStorage> {
        self.weak.lock().clone()
    }

    pub fn strong_oops_do<F>(&self, mut f: F)
    where
        F: FnMut(*mut Value<'static>),
    {
        for storage in self.strong_storages() {
            storage.oops_do(&mut f);
        }
    }

    pub fn scan_strong(&self, factory: &mut impl RootsWorkFactory<ObjectSlot>) {
        for storage in self.strong_storages() {
            storage.scan_roots(factory);
        }
    }

    pub fn scan_weak<A>(&self, mut is_alive: A, factory: &mut impl RootsWorkFactory<ObjectSlot>)
    where
        A: FnMut(Value<'static>) -> bool,
    {
        for storage in self.weak_storages() {
            storage.scan_weak_roots(&mut is_alive, factory);
        }
    }

    /// Run empty-block cleanup across all storages; returns true if more work may remain.
    pub fn delete_empty_blocks(&self) -> bool {
        let mut more = false;
        for storage in self.strong_storages() {
            more |= storage.delete_empty_blocks();
        }
        for storage in self.weak_storages() {
            more |= storage.delete_empty_blocks();
        }
        more
    }

    pub fn print_containing(&self, addr: *const Value<'static>) -> Option<String> {
        for storage in self.strong_storages() {
            if let Some(s) = storage.print_containing(addr) {
                return Some(s);
            }
        }
        for storage in self.weak_storages() {
            if let Some(s) = storage.print_containing(addr) {
                return Some(s);
            }
        }
        None
    }
}
