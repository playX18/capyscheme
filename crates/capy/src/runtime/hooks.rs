//! Runtime hook slots.
//!
//! Scheme code installs procedures and record types here with
//! `install-runtime-hook!` at boot, and Rust code reads them back by [`Hook`]
//! instead of looking them up by name. The slots are GC roots held by
//! [`Globals`](crate::runtime::global::Globals).

use std::cell::Cell;

use crate::heap::{Gc, Trace, cell::Lock};
use crate::prelude::*;
use crate::runtime::{MutationContext, record::Rtd};

/// Every runtime hook. The Scheme name of each is given by [`Hook::name`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Hook {
    Raise,
    RaiseContinuable,
    WithExceptionHandler,
    SlotRefUsingClass,
    SlotSetUsingClass,
    SlotBoundUsingClass,
    SlotUnbound,
    SlotMissing,
    SlotRef,
    SlotSet,
    AssertionViolation,
    UndefinedViolation,
    ImplementationRestrictionViolation,
    Error,
    RaiseIoError,
    RaiseIoFileDoesNotExistError,
    RaiseIoFileAlreadyExistsError,
    RaiseIoFileIsReadOnlyError,
    RaiseIoFileProtectionError,
    RaiseIoReadError,
    RaiseIoWriteError,
    MakeAssertionViolation,
    MakeUndefinedViolation,
    MakeError,
    MakeIoError,
    MakeLexicalViolation,
    TermTerm,
    TermLref,
    TermLset,
    TermIf,
    TermLet,
    TermFix,
    TermReceive,
    TermApplication,
    TermPrimcall,
    TermPrimref,
    TermConstant,
    TermVoid,
    TermValues,
    TermSequence,
    TermProc,
    TermWcm,
    TermGlobalRef,
    TermGlobalSet,
    TermLinklet,
}

const HOOK_COUNT: usize = 45;

impl Hook {
    /// Every hook, in slot order.
    pub const ALL: [Hook; HOOK_COUNT] = [
        Hook::Raise,
        Hook::RaiseContinuable,
        Hook::WithExceptionHandler,
        Hook::SlotRefUsingClass,
        Hook::SlotSetUsingClass,
        Hook::SlotBoundUsingClass,
        Hook::SlotUnbound,
        Hook::SlotMissing,
        Hook::SlotRef,
        Hook::SlotSet,
        Hook::AssertionViolation,
        Hook::UndefinedViolation,
        Hook::ImplementationRestrictionViolation,
        Hook::Error,
        Hook::RaiseIoError,
        Hook::RaiseIoFileDoesNotExistError,
        Hook::RaiseIoFileAlreadyExistsError,
        Hook::RaiseIoFileIsReadOnlyError,
        Hook::RaiseIoFileProtectionError,
        Hook::RaiseIoReadError,
        Hook::RaiseIoWriteError,
        Hook::MakeAssertionViolation,
        Hook::MakeUndefinedViolation,
        Hook::MakeError,
        Hook::MakeIoError,
        Hook::MakeLexicalViolation,
        Hook::TermTerm,
        Hook::TermLref,
        Hook::TermLset,
        Hook::TermIf,
        Hook::TermLet,
        Hook::TermFix,
        Hook::TermReceive,
        Hook::TermApplication,
        Hook::TermPrimcall,
        Hook::TermPrimref,
        Hook::TermConstant,
        Hook::TermVoid,
        Hook::TermValues,
        Hook::TermSequence,
        Hook::TermProc,
        Hook::TermWcm,
        Hook::TermGlobalRef,
        Hook::TermGlobalSet,
        Hook::TermLinklet,
    ];

    /// The name Scheme code installs this hook under.
    pub const fn name(self) -> &'static str {
        match self {
            Hook::Raise => "raise",
            Hook::RaiseContinuable => "raise-continuable",
            Hook::WithExceptionHandler => "with-exception-handler",
            Hook::SlotRefUsingClass => "slot-ref-using-class",
            Hook::SlotSetUsingClass => "slot-set-using-class!",
            Hook::SlotBoundUsingClass => "slot-bound-using-class?",
            Hook::SlotUnbound => "slot-unbound",
            Hook::SlotMissing => "slot-missing",
            Hook::SlotRef => "slot-ref",
            Hook::SlotSet => "slot-set!",
            Hook::AssertionViolation => "assertion-violation",
            Hook::UndefinedViolation => "undefined-violation",
            Hook::ImplementationRestrictionViolation => "implementation-restriction-violation",
            Hook::Error => "error",
            Hook::RaiseIoError => "raise-i/o-error",
            Hook::RaiseIoFileDoesNotExistError => "raise-i/o-file-does-not-exist-error",
            Hook::RaiseIoFileAlreadyExistsError => "raise-i/o-file-already-exists-error",
            Hook::RaiseIoFileIsReadOnlyError => "raise-i/o-file-is-read-only-error",
            Hook::RaiseIoFileProtectionError => "raise-i/o-file-protection-error",
            Hook::RaiseIoReadError => "raise-i/o-read-error",
            Hook::RaiseIoWriteError => "raise-i/o-write-error",
            Hook::MakeAssertionViolation => "%make-assertion-violation",
            Hook::MakeUndefinedViolation => "%make-undefined-violation",
            Hook::MakeError => "%make-error",
            Hook::MakeIoError => "%make-io-error",
            Hook::MakeLexicalViolation => "%make-lexical-violation",
            Hook::TermTerm => "&term",
            Hook::TermLref => "&lref",
            Hook::TermLset => "&lset",
            Hook::TermIf => "&if",
            Hook::TermLet => "&let",
            Hook::TermFix => "&fix",
            Hook::TermReceive => "&receive",
            Hook::TermApplication => "&application",
            Hook::TermPrimcall => "&primcall",
            Hook::TermPrimref => "&primref",
            Hook::TermConstant => "&constant",
            Hook::TermVoid => "&void",
            Hook::TermValues => "&values",
            Hook::TermSequence => "&sequence",
            Hook::TermProc => "&proc",
            Hook::TermWcm => "&wcm",
            Hook::TermGlobalRef => "&global-ref",
            Hook::TermGlobalSet => "&global-set",
            Hook::TermLinklet => "&linklet",
        }
    }

    /// The hook installed under the Scheme name `name`, if any.
    pub fn from_name(name: &str) -> Option<Hook> {
        Hook::ALL.into_iter().find(|hook| hook.name() == name)
    }

    /// Whether the hook must hold a record type descriptor.
    pub const fn is_rtd(self) -> bool {
        matches!(
            self,
            Hook::TermTerm
                | Hook::TermLref
                | Hook::TermLset
                | Hook::TermIf
                | Hook::TermLet
                | Hook::TermFix
                | Hook::TermReceive
                | Hook::TermApplication
                | Hook::TermPrimcall
                | Hook::TermPrimref
                | Hook::TermConstant
                | Hook::TermVoid
                | Hook::TermValues
                | Hook::TermSequence
                | Hook::TermProc
                | Hook::TermWcm
                | Hook::TermGlobalRef
                | Hook::TermGlobalSet
                | Hook::TermLinklet
        )
    }
}

/// One Value slot per [`Hook`], each starting as `#f`.
#[derive(Trace)]
pub struct Hooks<'gc> {
    slots: [Lock<Value<'gc>>; HOOK_COUNT],
}

impl<'gc> Hooks<'gc> {
    /// A table with every slot set to `#f`.
    pub fn new() -> Self {
        Self {
            slots: std::array::from_fn(|_| Lock::new(Value::new(false))),
        }
    }

    /// The current value of `hook`, `#f` when not installed.
    pub fn get(&self, hook: Hook) -> Value<'gc> {
        self.slots[hook as usize].get()
    }

    /// Stores `value` in the slot for `hook`.
    pub(crate) fn install(&self, hook: Hook, value: Value<'gc>) {
        // SAFETY: `Hooks` lives inside `Globals`, which is a GC root traced in
        // full on every collection, so the write needs no barrier.
        unsafe { self.slots[hook as usize].as_cell().set(value) };
    }

    /// Calls `f` on every slot, for code that walks the globals' value cells.
    pub fn for_each_slot(&self, mut f: impl FnMut(&Cell<Value<'gc>>)) {
        for slot in &self.slots {
            // SAFETY: the reference is only passed to `f` and never stored.
            // Writes go through `install`, which upholds the same root
            // invariant.
            f(unsafe { slot.as_cell() });
        }
    }
}

impl<'gc> Default for Hooks<'gc> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'gc> MutationContext<'gc> {
    /// The value installed for `hook`, or `None` when the slot still holds `#f`.
    pub fn try_hook(self, hook: Hook) -> Option<Value<'gc>> {
        let value = self.globals().hooks.get(hook);
        (value != Value::new(false)).then_some(value)
    }

    /// The value installed for `hook`. Panics if boot has not installed it.
    pub fn hook(self, hook: Hook) -> Value<'gc> {
        self.try_hook(hook).unwrap_or_else(|| {
            panic!(
                "runtime hook '{}' is not installed (boot has not finished)",
                hook.name()
            )
        })
    }

    /// The hook for record type descriptor
    pub fn hook_rtd(self, hook: Hook) -> Gc<'gc, Rtd<'gc>> {
        let value = self.hook(hook);
        assert!(
            value.is::<Rtd>(),
            "runtime hook '{}' is not a record type descriptor",
            hook.name()
        );
        value.downcast::<Rtd>()
    }
}

/// Raises an assertion violation attributed to `who`
fn violation<'gc>(
    call: &Call<'_, 'gc>,
    who: &str,
    message: &str,
    irritant: Value<'gc>,
) -> Value<'gc> {
    let ctx = call.ctx;
    let message = Str::new(ctx, message, true).into();
    call.raise_assertion_violation(ctx.intern(who), message, irritant)
}

const INSTALL_HOOK: &str = "install-runtime-hook!";
const INSTALL_EXPANDER_HOOK: &str = "install-expander-hook!";
const REF_HOOK: &str = "runtime-hook-ref";

/// Stores `value` in the hook `name`.
fn install_hook<'gc>(
    call: &Call<'_, 'gc>,
    ctx: MutationContext<'gc>,
    who: &str,
    name: Value<'gc>,
    value: Value<'gc>,
    rtd: bool,
) -> Value<'gc> {
    if !name.is::<Symbol>() {
        return violation(call, who, "expected a symbol for hook name", name);
    }
    let symbol = name.downcast::<Symbol>();
    let text = symbol.as_str();
    let Some(hook) = Hook::from_name(&text) else {
        return violation(call, who, "unknown runtime hook", name);
    };
    if hook.is_rtd() != rtd {
        return violation(
            call,
            who,
            "hook is installed with the other primitive",
            name,
        );
    }
    if rtd && !value.is::<Rtd>() {
        return violation(
            call,
            who,
            "expected a record-type descriptor for hook",
            value,
        );
    }
    ctx.globals().hooks.install(hook, value);
    Value::undefined()
}

#[scheme(path=capy)]
pub mod hooks_ops {

    /// Installs `value` as the runtime hook named `name`
    #[scheme(name = "install-runtime-hook!")]
    pub fn install_runtime_hook(name: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        install_hook(call, ctx, INSTALL_HOOK, name, value, false)
    }

    /// Installs the record type descriptor `value` as hook for Rust to call in expander
    #[scheme(name = "install-expander-hook!")]
    pub fn install_expander_hook(name: Value<'gc>, value: Value<'gc>) -> Value<'gc> {
        install_hook(call, ctx, INSTALL_EXPANDER_HOOK, name, value, true)
    }

    /// The value installed under `name`, or `#f` if the hook is not installed.
    #[scheme(name = "runtime-hook-ref")]
    pub fn runtime_hook_ref(name: Value<'gc>) -> Value<'gc> {
        if !name.is::<Symbol>() {
            return violation(call, REF_HOOK, "expected a symbol for hook name", name);
        }
        let symbol = name.downcast::<Symbol>();
        let text = symbol.as_str();
        match Hook::from_name(&text) {
            Some(hook) => ctx.try_hook(hook).unwrap_or(Value::new(false)),
            None => violation(call, REF_HOOK, "unknown runtime hook", name),
        }
    }
}
