use std::{marker::PhantomData, sync::OnceLock};

use rsgc::{Root, Rootable, Trace, Visitor, sync::monitor::Monitor};

use crate::{
    prelude::{FromValue, IntoValue, Value},
    runtime::Context,
};

pub trait GlobalValue<'gc>: Trace + IntoValue<'gc> {}

type GlobalMap<'gc> = Vec<Value<'gc>>;

/// A table listing all global variables in the runtime.
///
/// Implemented as a vector of [`Value`]s, indexed by [`Global`]s which allows
/// us to serialize and deserialize it in a straightforward manner.
pub struct GlobalTable<'gc> {
    // TODO: Allow removing globals? Use a slab or similar data structure?
    globals: Monitor<GlobalMap<'gc>>,
}

unsafe impl<'gc> Trace for GlobalTable<'gc> {
    unsafe fn trace(&mut self, tracer: &mut Visitor<'_>) {
        for value in self.globals.get_mut().iter_mut() {
            tracer.trace(value);
        }
    }

    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl Send for GlobalTable<'_> {}
unsafe impl Sync for GlobalTable<'_> {}

static GLOBALS: OnceLock<rsgc::Global<Rootable!(GlobalTable<'_>)>> = OnceLock::new();

/// The main difference between [`rsgc::Global`] and this type
/// is that this type does not store pointer, but just an index into
/// the side global table. Global table itself stores [`Value`]s directly
/// and `Global` provides typed access to them.
pub struct Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
    index: usize,
    marker: PhantomData<*const R>,
}

pub fn globals<'gc>() -> &'static rsgc::Global<Rootable!(GlobalTable<'_>)> {
    GLOBALS.get_or_init(|| {
        rsgc::Global::new(GlobalTable {
            globals: Monitor::new(Vec::with_capacity(128)),
        })
    })
}

/// Set the global table to the given vector of values.
///
/// # Safety
///
/// While this function is not unsafe per-se, it can lead to wrong program
/// behaviour if it sets existing globals to incorrect indices.
pub unsafe fn globals_from_vec<'gc>(_ctx: impl AsRef<Context<'gc>>, vec: Vec<Value<'gc>>) {
    GLOBALS
        .set(rsgc::Global::new(GlobalTable {
            globals: Monitor::new(vec),
        }))
        .unwrap_or_else(|_| panic!("globals have already been initialized"));
}

pub fn for_each_global<'gc, F>(ctx: impl AsRef<Context<'gc>>, mut f: F)
where
    F: FnMut(usize, Value<'gc>),
{
    let ctx = ctx.as_ref();
    let globals = globals().fetch(ctx);
    let guard = globals.globals.lock();
    for (i, value) in guard.iter().enumerate() {
        f(i, *value);
    }
}

impl<R> Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
    /// Create a new global variable initialized to `initial`.
    pub fn new<'gc>(ctx: &Context<'gc>, initial: Root<'gc, R>) -> Self
    where
        Root<'gc, R>: IntoValue<'gc>,
    {
        let globals = globals().fetch(ctx);
        let mut guard = globals.globals.lock();
        let index = guard.len();
        guard.push(initial.into_value(*ctx));
        Self {
            index,
            marker: PhantomData,
        }
    }

    /// Get the value of the global variable.
    pub fn fetch<'gc>(&self, ctx: &Context<'gc>) -> Root<'gc, R>
    where
        for<'a> Root<'a, R>: FromValue<'a>,
    {
        let globals = globals().fetch(&ctx);
        let guard = globals.globals.lock();
        let value = guard[self.index];
        Root::<R>::try_from_value(*ctx, value).expect("global variable has wrong type")
    }

    /// Set the value of the global variable.
    pub fn set<'gc>(&self, ctx: impl AsRef<Context<'gc>>, value: Root<'gc, R>)
    where
        Root<'gc, R>: IntoValue<'gc>,
    {
        let ctx = ctx.as_ref();
        let globals = globals().fetch(ctx);
        let mut guard = globals.globals.lock();
        guard[self.index] = value.into_value(*ctx);
    }

    pub unsafe fn from_index(index: usize) -> Self {
        Self {
            index,
            marker: PhantomData,
        }
    }
}

unsafe impl<R> Send for Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
}

unsafe impl<R> Sync for Global<R>
where
    R: for<'a> Rootable<'a>,
    for<'a> Root<'a, R>: Sized + Trace,
{
}

#[macro_export]
macro_rules! global {
    ($($v: vis $name: ident <$l: lifetime>: $t: ty = ($ctx: ident) $init: expr;)*) => {
        $(
            paste::paste!{
                #[allow(unused)]
                #[unsafe(export_name = concat!("CAPY_GLOBAL_", stringify!($name)))]
                $v static [<$name: upper>]: std::sync::OnceLock<$crate::prelude::Global<$crate::rsgc::Rootable!($l => $t)>> = std::sync::OnceLock::new();

                #[allow(unused)]
                #[unsafe(export_name = concat!("capy_global_", stringify!($name)))]
                $v fn [<$name: lower>]<$l>($ctx: $crate::runtime::Context<$l>) -> $t {
                   [<$name: upper>]
                        .get_or_init(|| {
                            let init: $t = $init;
                            $crate::prelude::Global::new(&$ctx, init)
                        }).fetch(&$ctx)
                }


                #[allow(unused)]
                #[doc(hidden)]
                pub unsafe fn [<$name: lower _from_index>](index: usize) -> $crate::prelude::Global<$crate::rsgc::Rootable!($l => $t)> {
                    unsafe {
                        $crate::prelude::Global::from_index(index)
                    }
                }


            }
        )*
    };
}
