use std::cell::Cell;

use rsgc::{Gc, Mutation, Mutator, Rootable, Trace, mmtk::util::Address};

use crate::runtime::{
    fluids::DynamicState,
    value::{
        NativeReturn, ReturnCode, SavedCall, Value, init_symbols, init_weak_sets, init_weak_tables,
    },
    vm::{VMResult, call_scheme},
};

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Context<'gc> {
    pub(crate) mc: &'gc Mutation<'gc>,
    pub(crate) state: &'gc State<'gc>,
}

impl<'gc> From<(&'gc Mutation<'gc>, &'gc State<'gc>)> for Context<'gc> {
    fn from((mc, state): (&'gc Mutation<'gc>, &'gc State<'gc>)) -> Self {
        Self { mc, state }
    }
}

impl<'gc> Context<'gc> {
    pub fn new(mc: &'gc Mutation<'gc>, state: &'gc State<'gc>) -> Self {
        Self { mc, state }
    }

    pub fn mutation(&self) -> &'gc Mutation<'gc> {
        self.mc
    }

    pub fn state(&self) -> &'gc State<'gc> {
        self.state
    }

    pub fn has_suspended_call(&self) -> bool {
        self.state.saved_call.get().is_some()
    }

    pub fn resume_suspended_call(&self) -> VMResult<'gc> {
        let call = self
            .state
            .saved_call
            .replace(None)
            .expect("No suspended call");

        call_scheme(*self, call.rator, &call.rands)
    }

    pub fn return_call(
        &self,
        rator: Value<'gc>,
        rands: impl IntoIterator<Item = Value<'gc>>,
        conts: Option<[Value<'gc>; 2]>,
    ) -> NativeReturn<'gc> {
        let rands_ptr = self.state.runstack.get().to_mut_ptr::<Value>();
        let disp = if conts.is_some() { 2 } else { 0 };
        unsafe {
            if let Some(conts) = conts {
                *rands_ptr = conts[0];
                *(rands_ptr.add(1)) = conts[1];
            }
            let mut count = 0;
            for (i, rand) in rands.into_iter().enumerate() {
                *rands_ptr.add(disp + i) = rand;
                count += 1;
            }

            self.state
                .runstack
                .set(Address::from_ptr(rands_ptr.add(count)));
            self.state.call_data.rands.set(rands_ptr);
            self.state.call_data.num_rands.set(count + disp);
            self.state.call_data.rator.set(rator);
        }

        NativeReturn {
            code: ReturnCode::Continue,
            value: Value::new(false),
        }
    }
}

impl<'gc> std::ops::Deref for Context<'gc> {
    type Target = Mutation<'gc>;

    fn deref(&self) -> &Self::Target {
        self.mc
    }
}

/// Hardcoded limit for runstack to not make calls too slow and runstack too large.
const RUNSTACK_SIZE: usize = 4096;

pub struct State<'gc> {
    pub(crate) dynamic_state: DynamicState<'gc>,
    pub(crate) runstack: Cell<Address>,
    pub(crate) runstack_start: Address,
    pub(crate) call_data: CallData<'gc>,
    pub(crate) saved_call: Cell<Option<Gc<'gc, SavedCall<'gc>>>>,
}

#[repr(C)]
pub struct CallData<'gc> {
    pub rator: Cell<Value<'gc>>,
    pub rands: Cell<*mut Value<'gc>>,
    pub num_rands: Cell<usize>,
}

unsafe impl<'gc> Trace for CallData<'gc> {
    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        visitor.trace(&mut self.rator);
        unsafe {
            if !self.rands.get().is_null() {
                for i in 0..self.num_rands.get() {
                    (*self.rands.get().add(i)).trace(visitor);
                }
            }
        }
    }
    unsafe fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

unsafe impl Trace for State<'_> {
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}

    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        visitor.trace(&mut self.dynamic_state);

        let runstack = unsafe {
            std::slice::from_raw_parts_mut(
                self.runstack_start.to_mut_ptr::<Value>(),
                (self.runstack.get() - self.runstack_start) / size_of::<Value>(),
            )
        };

        for value in runstack {
            visitor.trace(value);
        }
    }
}

impl<'gc> State<'gc> {
    pub fn new(mc: &'gc Mutation<'gc>) -> Self {
        let (runstack_start, _runstack_end) = make_fresh_runstack();
        Self {
            dynamic_state: DynamicState::new(mc),
            runstack: Cell::new(runstack_start),

            runstack_start,
            call_data: CallData {
                rator: Cell::new(Value::undefined()),
                rands: Cell::new(std::ptr::null_mut()),
                num_rands: Cell::new(0),
            },
            saved_call: Cell::new(None),
        }
    }

    pub fn context(&'gc self, mc: &'gc Mutation<'gc>) -> Context<'gc> {
        Context { mc, state: self }
    }
}

pub struct Scheme {
    pub mutator: Mutator<Rootable!(State<'_>)>,
}

impl Scheme {
    pub fn enter<F, T>(&self, f: F) -> T
    where
        F: for<'gc> FnOnce(Context<'gc>) -> T,
    {
        let (result, should_gc) = self.mutator.mutate(|mc, state| {
            let result = f(state.context(mc));

            (result, mc.take_yieldpoint() != 0)
        });

        if should_gc {
            self.mutator.collect_garbage();
        }

        result
    }

    pub fn new() -> Self {
        Self {
            mutator: {
                let m = Mutator::new(|mc| {
                    init_weak_sets(&mc);
                    init_weak_tables(&mc);
                    init_symbols(&mc);

                    State::new(mc)
                });
                m.mutate(|mc, state: &mut State<'_>| {
                    super::init(state.context(mc));
                });
                m
            },
        }
    }
}

impl<'gc> Drop for State<'gc> {
    fn drop(&mut self) {
        unsafe {
            let layout = std::alloc::Layout::from_size_align(
                RUNSTACK_SIZE * std::mem::size_of::<Value>(),
                std::mem::align_of::<Value>(),
            )
            .unwrap();
            std::alloc::dealloc(self.runstack_start.to_mut_ptr() as *mut u8, layout);
        }
    }
}

fn make_fresh_runstack() -> (Address, Address) {
    let layout = std::alloc::Layout::from_size_align(
        RUNSTACK_SIZE * std::mem::size_of::<Value>(),
        std::mem::align_of::<Value>(),
    )
    .unwrap();
    unsafe {
        let ptr = std::alloc::alloc(layout) as *mut Value;
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        let start = Address::from_ptr(ptr);
        let end = Address::from_ptr(ptr.add(RUNSTACK_SIZE));
        (start, end)
    }
}
