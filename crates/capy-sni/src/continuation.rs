//! Continuation trampoline wrappers for SNI.

use std::ptr;

use capy_sni_sys as sys;

use crate::{Env, Ref, ScmRef};

/// Opaque owned continuation (`sni_cont_alloc` / `sni_cont_free`).
pub struct Continuation {
    ptr: *mut sys::SniContinuation,
}

// Field accesses below go through `sys::SniContinuation`, which mirrors
// `struct SniContinuation` in c/capy.h exactly (pinned by const asserts in
// capy-sni-sys); conts are allocated by sni_cont_alloc and stay live while
// this `Continuation` (or the C trampoline, after into_raw) owns them.

impl Continuation {
    pub fn as_ptr(&self) -> *mut sys::SniContinuation {
        self.ptr
    }

    pub fn from_raw(ptr: *mut sys::SniContinuation) -> Self {
        Self { ptr }
    }

    pub fn into_raw(self) -> *mut sys::SniContinuation {
        let p = self.ptr;
        std::mem::forget(self);
        p
    }

    /// Continuation-owned global root slot (`c->result`). GC-safe for the
    /// lifetime of this `Continuation`; do not free the cont while using it.
    pub fn result(&self) -> ScmRef {
        if self.ptr.is_null() {
            return ptr::null_mut();
        }
        // SAFETY: see the impl-level layout note.
        unsafe { (*self.ptr).result }
    }

    /// Copy `value`'s Scheme object into the continuation-owned result root.
    /// Does not replace the slot pointer (safe across local-frame pops).
    pub fn set_result(&mut self, env: &mut Env<'_>, value: Ref<'_>) {
        if self.ptr.is_null() {
            return;
        }
        // SAFETY: env is live by branding; cont is live (see impl-level note).
        unsafe {
            sys::sni_cont_set_result(env.as_ptr(), self.ptr, value.as_raw());
        }
    }

    pub fn set_next(&mut self, next: sys::SniContFn) {
        if self.ptr.is_null() {
            return;
        }
        // SAFETY: see the impl-level layout note.
        unsafe {
            (*self.ptr).next = next;
        }
    }

    pub fn finish(&mut self) {
        self.set_next(None);
    }

    pub fn data_ptr(&self) -> *mut libc::c_void {
        if self.ptr.is_null() {
            return ptr::null_mut();
        }
        // SAFETY: see the impl-level layout note.
        unsafe { (*self.ptr).data }
    }
}

impl Drop for Continuation {
    fn drop(&mut self) {
        if !self.ptr.is_null() {
            // SAFETY: cont is owned by self and freed exactly once.
            unsafe {
                sys::sni_cont_free(ptr::null_mut(), self.ptr);
            }
            self.ptr = ptr::null_mut();
        }
    }
}

impl<'env> Env<'env> {
    /// Allocate a continuation. `data` is passed back to `start` / resumed
    /// legs and to `drop_data` on free.
    ///
    /// # Safety
    /// `data` must stay valid until the continuation is freed (and
    /// `drop_data`, if any, has run).
    pub unsafe fn cont_alloc(
        &mut self,
        start: sys::SniContFn,
        data: *mut libc::c_void,
        drop_data: sys::SniDropDataFn,
    ) -> Continuation {
        // SAFETY: standing invariants (see `Env`); data validity is on the caller.
        let ptr = unsafe { sys::sni_cont_alloc(self.as_ptr(), start, data, drop_data) };
        Continuation { ptr }
    }

    pub fn trampoline(&mut self, c: &mut Continuation) -> Ref<'env> {
        // SAFETY: standing invariants; result is a fresh local ref.
        unsafe { Ref::from_raw(sys::sni_trampoline(self.as_ptr(), c.as_ptr())) }
    }

    pub fn call_k(&mut self, proc: Ref<'_>, args: &[Ref<'_>], c: &mut Continuation) {
        // SAFETY: standing invariants; args is a &[Ref] and Ref is
        // repr(transparent) over ScmRef.
        unsafe {
            sys::sni_call_k(
                self.as_ptr(),
                proc.as_raw(),
                args.len() as i32,
                args.as_ptr() as *const ScmRef,
                c.as_ptr(),
            );
        }
    }

    pub fn raise_k(&mut self, obj: Ref<'_>, c: &mut Continuation) {
        // SAFETY: standing invariants.
        unsafe { sys::sni_raise_k(self.as_ptr(), obj.as_raw(), c.as_ptr()) };
    }

    pub fn guard_k(&mut self, handler: &mut Continuation, body: &mut Continuation) -> Ref<'env> {
        // SAFETY: standing invariants.
        let finished = unsafe { sys::sni_guard_k(self.as_ptr(), handler.as_ptr(), body.as_ptr()) };
        if finished.is_null() {
            return Ref::null();
        }
        // SAFETY: finished is a live cont; its result slot is a rooted handle
        // (see the impl-level layout note on `Continuation`).
        unsafe { Ref::from_raw((*finished).result) }
    }
}

/// Colorless magic ops — only valid inside `#[cps]` (macro rewrites these).
pub mod cps_ops {
    use crate::Ref;

    /// Suspend: call Scheme procedure and resume with result.
    pub fn call<'a>(_proc: Ref<'a>, _args: &[Ref<'a>]) -> Ref<'a> {
        panic!(
            "`cps::call` used outside a #[cps] function (or via an import path the macro does not recognize)"
        )
    }

    /// Suspend: raise into Scheme.
    pub fn raise<'a>(_obj: Ref<'a>) -> ! {
        panic!(
            "`cps::raise` used outside a #[cps] function (or via an import path the macro does not recognize)"
        )
    }

    /// Suspend: run body under exception handler (both sides are CPS-transformed).
    pub fn guard<'a, H, B, R>(_handler: H, _body: B) -> R
    where
        H: FnOnce(Ref<'a>) -> R,
        B: FnOnce() -> R,
    {
        panic!(
            "`cps::guard` used outside a #[cps] function (or via an import path the macro does not recognize)"
        )
    }
}

/// Runtime helpers used by the `#[cps]` expansion (not for direct use).
#[doc(hidden)]
pub mod __cps {
    use std::cell::RefCell;
    use std::panic::{AssertUnwindSafe, catch_unwind};
    use std::rc::Rc;

    use super::*;
    use crate::Global;

    pub type Leg<'env> =
        Box<dyn FnMut(&mut Env<'env>, &mut Continuation) -> *mut sys::SniContinuation + 'env>;

    pub type SlotId = usize;

    pub struct Data<'env, R> {
        pub pending: Option<Leg<'env>>,
        pub result: Option<R>,
        /// Continuation-local GC roots (global refs) for `Ref`s live across suspend.
        pub roots: Vec<ScmRef>,
    }

    unsafe extern "C" fn drop_data<R>(data: *mut libc::c_void) {
        if !data.is_null() {
            // SAFETY: paired with Box::into_raw in `run`.
            let mut boxed = unsafe { Box::from_raw(data as *mut Data<'_, R>) };
            // Best-effort: env is unavailable on free; leak slots rather than
            // call delete without env. Cont free normally runs after `run`
            // clears roots via `release_roots`.
            boxed.roots.clear();
            drop(boxed);
        }
    }

    /// Root `value` in the continuation's slot table; returns an id for [`reload_ref`].
    pub fn root_ref<'env, R: 'static>(
        env: &mut Env<'env>,
        c: &mut Continuation,
        value: Ref<'_>,
    ) -> SlotId {
        let data = c.data_ptr() as *mut Data<'env, R>;
        let g = env.new_global_ref(value);
        let raw = g.into_raw();
        // SAFETY: data is the Data installed by `run`.
        unsafe {
            let id = (*data).roots.len();
            (*data).roots.push(raw);
            id
        }
    }

    /// Reload a previously rooted `Ref` after suspend / GC.
    ///
    /// # Safety
    /// `id` must come from [`root_ref`] on this continuation; env must be live.
    pub unsafe fn reload_ref<'env, R: 'static>(
        _env: &Env<'env>,
        c: &Continuation,
        id: SlotId,
    ) -> Ref<'env> {
        let data = c.data_ptr() as *mut Data<'env, R>;
        // SAFETY: id is a valid index into roots for this cont.
        let roots = unsafe { &(*data).roots };
        let raw = *roots.get(id).expect("cps reload: bad slot");
        unsafe { Ref::from_raw(raw) }
    }

    /// Delete all cont-local global roots (call before discarding `Data`).
    pub fn release_roots<'env, R: 'static>(env: &Env<'env>, c: &Continuation) {
        let data = c.data_ptr() as *mut Data<'env, R>;
        if data.is_null() {
            return;
        }
        // SAFETY: data owned by run until detach.
        let roots = unsafe { std::mem::take(&mut (*data).roots) };
        for raw in roots {
            if !raw.is_null() {
                Global(raw).delete(env);
            }
        }
    }

    unsafe extern "C" fn resume<R>(
        env: *mut sys::SniEnv,
        c: *mut sys::SniContinuation,
    ) -> *mut sys::SniContinuation {
        let outcome = catch_unwind(AssertUnwindSafe(|| {
            // SAFETY: env is live for the duration of this resume callback
            // (SNI continuation calling convention).
            let mut env = unsafe { Env::from_raw(env) };
            let mut cont = Continuation::from_raw(c);
            let data = cont.data_ptr() as *mut Data<'_, R>;
            // SAFETY: data is the Box::into_raw'd Data installed by `run`;
            // the cont owns it until `run` detaches it.
            let out = unsafe {
                if let Some(leg) = (*data).pending.as_mut() {
                    leg(&mut env, &mut cont)
                } else {
                    cont.finish();
                    cont.as_ptr()
                }
            };
            std::mem::forget(cont);
            out
        }));
        match outcome {
            Ok(out) => out,
            Err(p) => {
                eprintln!(
                    "capy-sni: panic in #[cps] continuation leg: {}; \
                     aborting (cannot unwind across the FFI boundary)",
                    crate::panic_payload_message(&*p)
                );
                std::process::abort()
            }
        }
    }

    /// Run a CPS graph; `start` is the first leg. Returns the value passed to [`finish`].
    pub fn run<'env, R: 'static>(env: &mut Env<'env>, mut start: Leg<'env>) -> R {
        let data = Box::into_raw(Box::new(Data::<'env, R> {
            pending: None,
            result: None,
            roots: Vec::new(),
        }));
        // SAFETY: data is owned by the cont until run detaches or frees it.
        let mut c = unsafe {
            env.cont_alloc(
                Some(resume::<R>),
                data as *mut libc::c_void,
                Some(drop_data::<R>),
            )
        };
        let first = start(env, &mut c);
        if first.is_null() {
            release_roots::<R>(env, &c);
            // SAFETY: the trampoline was dismissed synchronously; run owns data.
            let boxed = unsafe { Box::from_raw(data) };
            c.into_raw();
            return boxed
                .result
                .expect("#[cps] trampoline dismissed without result");
        }
        // SAFETY: layout mirror (see the impl-level note on `Continuation`).
        if unsafe { (*c.as_ptr()).next.is_none() } {
            release_roots::<R>(env, &c);
            // SAFETY: run owns data.
            let boxed = unsafe { Box::from_raw(data) };
            // Detach data from cont so Drop doesn't double-free.
            unsafe {
                (*c.as_ptr()).data = ptr::null_mut();
                (*c.as_ptr()).drop_data = None;
            }
            return boxed.result.expect("#[cps] finished without result");
        }
        let _ = env.trampoline(&mut c);
        release_roots::<R>(env, &c);
        unsafe {
            (*c.as_ptr()).data = ptr::null_mut();
            (*c.as_ptr()).drop_data = None;
        }
        // SAFETY: the trampoline ran to completion; run owns data.
        let boxed = unsafe { Box::from_raw(data) };
        boxed
            .result
            .expect("#[cps] trampoline finished without result")
    }

    pub fn finish<R: 'static>(c: &mut Continuation, value: R) -> *mut sys::SniContinuation {
        let data = c.data_ptr() as *mut Data<'_, R>;
        // SAFETY: data is the Data installed by `run`.
        unsafe {
            (*data).result = Some(value);
            (*data).pending = None;
        }
        c.finish();
        c.as_ptr()
    }

    pub fn suspend_call<'env, R: 'static>(
        env: &mut Env<'env>,
        proc: Ref<'_>,
        args: &[Ref<'_>],
        c: &mut Continuation,
        then: Leg<'env>,
    ) -> *mut sys::SniContinuation {
        env.call_k(proc, args, c);
        let data = c.data_ptr() as *mut Data<'env, R>;
        // SAFETY: data is the Data installed by `run`.
        unsafe {
            (*data).pending = Some(then);
        }
        c.set_next(Some(resume::<R>));
        c.as_ptr()
    }

    pub fn suspend_raise<'env, R: 'static>(
        env: &mut Env<'env>,
        obj: Ref<'_>,
        c: &mut Continuation,
        then: Leg<'env>,
    ) -> *mut sys::SniContinuation {
        env.raise_k(obj, c);
        let data = c.data_ptr() as *mut Data<'env, R>;
        // SAFETY: data is the Data installed by `run`.
        unsafe {
            (*data).pending = Some(then);
        }
        c.set_next(Some(resume::<R>));
        c.as_ptr()
    }

    /// Schedule `slot`'s leg as `c.next` and return to the trampoline.
    pub fn yield_to<'env, R: 'static>(
        c: &mut Continuation,
        slot: &Rc<RefCell<Option<Leg<'env>>>>,
    ) -> *mut sys::SniContinuation {
        let slot = slot.clone();
        let data = c.data_ptr() as *mut Data<'env, R>;
        // SAFETY: data is the Data installed by `run`.
        unsafe {
            (*data).pending = Some(Box::new(move |env, c| {
                let mut borrow = slot.borrow_mut();
                let leg = borrow.as_mut().expect("cps yield_to: empty slot");
                leg(env, c)
            }));
        }
        c.set_next(Some(resume::<R>));
        c.as_ptr()
    }

    pub fn new_slot<'env>() -> Rc<RefCell<Option<Leg<'env>>>> {
        Rc::new(RefCell::new(None))
    }

    pub fn set_slot<'env>(slot: &Rc<RefCell<Option<Leg<'env>>>>, leg: Leg<'env>) {
        *slot.borrow_mut() = Some(leg);
    }
}
