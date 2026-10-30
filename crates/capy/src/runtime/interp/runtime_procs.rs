//! Procedures the runtime provides to out-of-line slow paths.

use std::sync::OnceLock;

use crate::heap::{Global, Trace};
use crate::runtime::hooks::Hook;
use crate::runtime::{MutationContext, value::Value};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum RuntimeProc {
    SlotRef = 0,
    SlotSet = 1,
}

impl RuntimeProc {
    pub const COUNT: u32 = 2;

    pub fn name(self) -> &'static str {
        match self {
            Self::SlotRef => "slot-ref",
            Self::SlotSet => "slot-set!",
        }
    }

    pub fn from_id(id: u32) -> Option<Self> {
        match id {
            0 => Some(Self::SlotRef),
            1 => Some(Self::SlotSet),
            _ => None,
        }
    }
}

#[derive(Trace)]
#[collect(no_drop)]
struct RuntimeProcTable<'gc> {
    procs: [Value<'gc>; RuntimeProc::COUNT as usize],
}

type RootedRuntimeProcs = crate::Rootable!(RuntimeProcTable<'_>);

static RUNTIME_PROCS: OnceLock<Global<RootedRuntimeProcs>> = OnceLock::new();

/// The procedure for `id`, resolved in the root module on first use.
///
/// # Panics
///
/// When `id` is not a [`RuntimeProc`] or the procedure is not bound, which
/// only happens before boot completed.
pub fn get<'gc>(ctx: MutationContext<'gc>, id: u32) -> Value<'gc> {
    let Some(proc) = RuntimeProc::from_id(id) else {
        panic!("unknown runtime procedure id {id}");
    };
    let table = RUNTIME_PROCS.get_or_init(|| {
        let slot_ref = ctx.hook(Hook::SlotRef);
        let slot_set = ctx.hook(Hook::SlotSet);
        Global::new(RuntimeProcTable {
            procs: [slot_ref, slot_set],
        })
    });
    table.fetch(ctx).procs[proc as usize]
}
