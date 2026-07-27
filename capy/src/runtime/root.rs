//! Fast precise root stack for Values held across `call_in_native` / GC.
//!
//! Unlike the debug [`crate::runtime::vm::debug::ShadowStack`], this is a
//! relocatable rooting API: slots are traced by [`crate::runtime::State`] and
//! must be reloaded after any safepoint. Prefer OopStorage / [`crate::runtime::jni`]
//! for nest and longer-lived handles.

use std::cell::Cell;

use crate::rsgc::Trace;
use crate::runtime::{Context, State, value::Value};

/// Growable precise root array of relocatable [`Value`] slots.
pub struct RootStack<'gc> {
    slots: UnsafeCellSlots<'gc>,
    len: Cell<usize>,
}

/// Interior mutability for slot storage without requiring `&mut` during push.
struct UnsafeCellSlots<'gc> {
    inner: std::cell::UnsafeCell<Vec<Cell<Value<'gc>>>>,
}

impl<'gc> RootStack<'gc> {
    pub fn new() -> Self {
        Self {
            slots: UnsafeCellSlots {
                inner: std::cell::UnsafeCell::new(Vec::with_capacity(16)),
            },
            len: Cell::new(0),
        }
    }

    fn slots(&self) -> &mut Vec<Cell<Value<'gc>>> {
        // SAFETY: RootStack is exclusively owned by the mutator thread; GC
        // tracing runs stop-the-world with no concurrent push/pop.
        unsafe { &mut *self.slots.inner.get() }
    }

    pub fn len(&self) -> usize {
        self.len.get()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Push `value` and return its slot index.
    pub fn push(&self, value: Value<'gc>) -> usize {
        let idx = self.len.get();
        let slots = self.slots();
        if idx < slots.len() {
            slots[idx].set(value);
        } else {
            slots.push(Cell::new(value));
        }
        self.len.set(idx + 1);
        idx
    }

    pub fn get(&self, index: usize) -> Value<'gc> {
        debug_assert!(index < self.len.get());
        self.slots()[index].get()
    }

    pub fn set(&self, index: usize, value: Value<'gc>) {
        debug_assert!(index < self.len.get());
        self.slots()[index].set(value);
    }

    /// Truncate the stack to `new_len` (clears dropped slots).
    pub fn truncate(&self, new_len: usize) {
        debug_assert!(new_len <= self.len.get());
        let slots = self.slots();
        for i in new_len..self.len.get() {
            slots[i].set(Value::undefined());
        }
        self.len.set(new_len);
    }
}

// SAFETY: slots are mutator-owned; GC traces live prefix only.
unsafe impl Trace for RootStack<'_> {
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut crate::rsgc::WeakProcessor) {}

    unsafe fn trace(&mut self, visitor: &mut crate::rsgc::collection::Visitor) {
        let len = self.len.get();
        let slots = unsafe { &mut *self.slots.inner.get() };
        for i in 0..len {
            visitor.trace(slots[i].get_mut());
        }
    }
}

/// RAII scope that pops roots pushed after construction when dropped.
pub struct RootScope<'a, 'gc> {
    pub stack: &'a RootStack<'gc>,
    base: usize,
}

impl<'a, 'gc> RootScope<'a, 'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        let stack = &ctx.state().root_stack;
        Self {
            base: stack.len(),
            stack,
        }
    }

    pub fn from_state(state: &'a State<'gc>) -> Self {
        Self {
            base: state.root_stack.len(),
            stack: &state.root_stack,
        }
    }

    pub fn root(&self, value: Value<'gc>) -> Rooted<'a, 'gc> {
        let index = self.stack.push(value);
        Rooted {
            stack: self.stack,
            index,
        }
    }

    pub fn root_gc<T: Trace + 'gc>(&self, gc: crate::rsgc::Gc<'gc, T>) -> Rooted<'a, 'gc>
    where
        crate::rsgc::Gc<'gc, T>: Into<Value<'gc>>,
    {
        self.root(gc.into())
    }
}

impl Drop for RootScope<'_, '_> {
    fn drop(&mut self) {
        self.stack.truncate(self.base);
    }
}

/// A single rooted [`Value`] slot; always reload via [`Self::get`] after GC.
pub struct Rooted<'a, 'gc> {
    stack: &'a RootStack<'gc>,
    index: usize,
}

impl<'a, 'gc> Rooted<'a, 'gc> {
    pub fn get(&self) -> Value<'gc> {
        self.stack.get(self.index)
    }

    pub fn set(&self, value: Value<'gc>) {
        self.stack.set(self.index, value);
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

/// Run `f` while `InNative`, with `values` rooted; returns `(f result, reloaded values)`.
pub fn with_rooted_native<'gc, const N: usize, T>(
    ctx: Context<'gc>,
    values: [Value<'gc>; N],
    f: impl FnOnce() -> T,
) -> (T, [Value<'gc>; N]) {
    let scope = RootScope::new(ctx);
    let mut indices = [0usize; N];
    for i in 0..N {
        indices[i] = scope.root(values[i]).index();
    }
    let result = ctx.call_in_native(f);
    let mut out = [Value::undefined(); N];
    for i in 0..N {
        out[i] = scope.stack.get(indices[i]);
    }
    drop(scope);
    (result, out)
}

/// Push roots for the given expressions and bind `Rooted` guards.
///
/// After any `call_in_native` / nest, reload with `.get()` — do not use the
/// original stack copies.
///
/// ```ignore
/// root_scope!(ctx, scope);
/// let retk = scope.root(nctx.retk);
/// ctx.call_in_native(|| { ... });
/// nctx.retk = retk.get();
/// ```
#[macro_export]
macro_rules! root_scope {
    ($ctx:expr, $scope:ident) => {
        let $scope = $crate::runtime::root::RootScope::new($ctx);
    };
}

/// Convenience: root several Values, run `call_in_native`, reload into the same names.
///
/// ```ignore
/// rooted_native!(ctx, [retk, buf] => {
///     // body; retk/buf not usable inside for GC Values
/// });
/// // retk, buf reloaded
/// ```
#[macro_export]
macro_rules! rooted_native {
    ($ctx:expr, [$($name:ident),+ $(,)?] => $body:block) => {{
        let __scope = $crate::runtime::root::RootScope::new($ctx);
        $(
            let __idx_$name = __scope.root($name).index();
        )+
        let __result = ($ctx).call_in_native(|| $body);
        $(
            $name = __scope.stack.get(__idx_$name);
        )+
        drop(__scope);
        __result
    }};
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Scheme;
    use crate::runtime::value::Str;

    #[test]
    fn root_stack_push_get_truncate() {
        Scheme::new_uninit().enter(|ctx| {
            let scope = RootScope::new(ctx);
            let a = scope.root(Value::null());
            let s = Str::from_str(*ctx, "hi");
            let b = scope.root(s.into());
            assert!(a.get().is_null());
            assert_eq!(b.get().downcast::<Str>().to_string(), "hi");
            drop(scope);
            assert_eq!(ctx.state().root_stack.len(), 0);
        });
    }

    #[test]
    fn with_rooted_native_survives_collect() {
        Scheme::new_uninit().enter(|ctx| {
            let s = Str::from_str(*ctx, "rooted");
            let v = s.into();
            let ((), [reloaded]) = with_rooted_native(ctx, [v], || {
                let _ = crate::rsgc::mutator::user_collect_garbage();
            });
            assert_eq!(reloaded.downcast::<Str>().to_string(), "rooted");
        });
    }
}
