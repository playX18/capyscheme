use std::marker::PhantomData;

use rsgc::{Mutator, Rootable, Trace, context::Mutation};

#[derive(Clone, Copy)]
pub struct Context<'gc> {
    mc: &'gc Mutation<'gc>,
}

impl<'gc> Context<'gc> {
    pub fn new(mc: &'gc Mutation<'gc>) -> Self {
        Self { mc }
    }

    pub fn mutation(self) -> &'gc Mutation<'gc> {
        self.mc
    }
}

impl<'gc> std::ops::Deref for Context<'gc> {
    type Target = Mutation<'gc>;

    fn deref(&self) -> &Self::Target {
        self.mc
    }
}

pub struct State<'gc> {
    marker: PhantomData<&'gc ()>,
}

unsafe impl<'gc> Trace for State<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        let _ = visitor;
    }

    fn process_weak_refs(&mut self, weak_processor: &mut rsgc::WeakProcessor) {
        let _ = weak_processor;
    }
}

impl<'gc> State<'gc> {
    pub fn new() -> Self {
        Self {
            marker: PhantomData,
        }
    }

    pub fn ctx(&self, mc: &'gc Mutation<'gc>) -> Context<'gc> {
        Context::new(mc)
    }
}

/// A Scheme execution environment for current thread.
pub struct Scheme {
    pub mutator: Mutator<Rootable!(State<'_>)>,
}

impl Scheme {
    pub fn enter<F, T>(&self, f: F) -> T
    where
        F: for<'gc> FnOnce(Context<'gc>) -> T,
    {
        self.mutator.mutate(|mc, state| f(state.ctx(mc)))
    }

    pub fn new() -> Self {
        Self {
            mutator: Mutator::new(|mc| {
                super::init(mc);

                State::new()
            }),
        }
    }
}
