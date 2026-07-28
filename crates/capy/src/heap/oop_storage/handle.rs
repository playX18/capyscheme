//! RAII handle API on top of [`OopStorage`].

use std::ptr::NonNull;

use crate::runtime::value::Value;

use super::storage::OopStorage;

/// Alias for the strong process storage type.
pub type StrongOopStorage = OopStorage;

/// A rooted Value handle living in [`OopStorage`].
pub struct OopHandle {
    slot: NonNull<Value<'static>>,
}

impl OopHandle {
    /// Allocate a slot in `storage` and store `value`.
    pub fn new(storage: &OopStorage, value: Value<'_>) -> Self {
        let ptr = storage.allocate().expect("OopStorage allocation failed");
        // SAFETY: freshly allocated empty slot.
        unsafe {
            *ptr = std::mem::transmute::<Value<'_>, Value<'static>>(value);
        }
        Self {
            slot: NonNull::new(ptr).expect("allocate returned non-null"),
        }
    }

    pub fn as_ptr(&self) -> *mut Value<'static> {
        self.slot.as_ptr()
    }

    pub fn get<'gc>(&self) -> Value<'gc> {
        // SAFETY: slot is live for this handle's lifetime.
        unsafe { std::mem::transmute::<Value<'static>, Value<'gc>>(*self.slot.as_ptr()) }
    }

    pub fn set<'gc>(&self, value: Value<'gc>) {
        // SAFETY: slot is live for this handle's lifetime.
        unsafe {
            *self.slot.as_ptr() = std::mem::transmute::<Value<'gc>, Value<'static>>(value);
        }
    }

    /// Clear and release the slot back to `storage`.
    pub fn release(self, storage: &OopStorage) {
        // SAFETY: we own the slot.
        unsafe {
            *self.slot.as_ptr() = Value::empty();
            storage.release(self.slot.as_ptr());
        }
        std::mem::forget(self);
    }
}

impl Drop for OopHandle {
    fn drop(&mut self) {
        // Clear so a forgotten release does not keep a referent alive forever
        // in an allocated slot; the slot itself stays allocated until process end.
        // Prefer [`OopHandle::release`] or [`HandleScope`].
        unsafe {
            *self.slot.as_ptr() = Value::empty();
        }
    }
}

/// RAII batch of handles released together against one storage.
pub struct HandleScope<'a> {
    storage: &'a OopStorage,
    handles: Vec<OopHandle>,
}

impl<'a> HandleScope<'a> {
    pub fn new(storage: &'a OopStorage) -> Self {
        Self {
            storage,
            handles: Vec::new(),
        }
    }

    pub fn root<'gc>(&mut self, value: Value<'gc>) -> &OopHandle {
        self.handles.push(OopHandle::new(self.storage, value));
        self.handles.last().expect("just pushed")
    }

    pub fn len(&self) -> usize {
        self.handles.len()
    }

    /// Ensure the scope can hold at least `capacity` additional refs without realloc churn.
    pub fn ensure_capacity(&mut self, capacity: usize) {
        self.handles.reserve(capacity);
    }
}

impl Drop for HandleScope<'_> {
    fn drop(&mut self) {
        while let Some(h) = self.handles.pop() {
            h.release(self.storage);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn handle_roundtrip() {
        let storage = OopStorage::create("handle-test");
        let h = OopHandle::new(&storage, Value::null());
        assert!(h.get().is_null());
        h.set(Value::undefined());
        assert!(h.get().is_undefined_or_null() && !h.get().is_null());
        h.release(&storage);
        assert_eq!(storage.allocation_count(), 0);
    }

    #[test]
    fn handle_scope_releases() {
        let storage = OopStorage::create("scope-test");
        {
            let mut scope = HandleScope::new(&storage);
            let _ = scope.root(Value::null());
            let _ = scope.root(Value::undefined());
            assert_eq!(storage.allocation_count(), 2);
        }
        assert_eq!(storage.allocation_count(), 0);
    }
}
