use rsgc::{Gc, Mutation, Mutator, Rootable, Trace};

use crate::runtime::{
    fluids::DynamicState,
    value::{
        HashTable, HashTableType, NativeProc, Return,
        Value, /*  Variable, current_environment, current_variable_environment*/
    },
    vm::VMState,
};

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Context<'gc> {
    mc: &'gc Mutation<'gc>,
    pub(crate) state: &'gc State<'gc>,
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
    /*
    pub fn lookup_current_environment(self, sym: impl IntoValue<'gc>) -> Option<Value<'gc>> {
        let env = current_environment(self);

        env.variables
            .get()
            .get(self, sym.into_value(self))
            .map(|v| v.downcast::<Variable>().get())
    }

    pub fn intern_current_environment(self, sym: impl IntoValue<'gc>, value: Value<'gc>) {
        let vars = current_variable_environment(self);

        let (inserted, var) = vars.get_or_insert(self, sym.into_value(self), |ctx| {
            Variable::new(ctx, value).into()
        });

        if !inserted {
            var.downcast::<Variable>().set(self, value);
        }
    }

    pub fn set_top_level_value(self, sym: impl IntoValue<'gc>, value: Value<'gc>) {
        self.intern_current_environment(sym, value);
    }

    pub fn is_top_level_bound(self, sym: impl IntoValue<'gc>) -> bool {
        let env = current_environment(self);
        env.variables.get().contains_key(self, sym.into_value(self))
    }*/

    pub fn call(
        self,
        proc: Value<'gc>,
        args: impl AsRef<[Value<'gc>]>,
    ) -> Result<Value<'gc>, Value<'gc>> {
        let _ = proc;
        let _ = args;

        if let Some(proc) = proc.try_as::<NativeProc>() {
            match (proc.proc)(self, args.as_ref(), proc.closure) {
                Return::Ok(value) => Ok(value),
                Return::Err(err) => Err(err),
            }
        } else {
            todo!()
        }
    }

    pub fn vm(&self) -> &VMState<'gc> {
        &self.state.vm_state
    }

    pub fn global_location(self, sym: Value<'gc>) -> Value<'gc> {
        self.state
            .globals
            .get_or_insert(self, sym, |ctx| Value::cons(ctx, Value::undefined(), sym))
            .1
    }
}

impl<'gc> std::ops::Deref for Context<'gc> {
    type Target = Mutation<'gc>;

    fn deref(&self) -> &Self::Target {
        self.mc
    }
}

pub struct State<'gc> {
    pub(crate) dynamic_state: DynamicState<'gc>,
    pub(crate) vm_state: VMState<'gc>,
    pub(crate) globals: Gc<'gc, HashTable<'gc>>,
}

unsafe impl Trace for State<'_> {
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}

    unsafe fn trace(&mut self, _visitor: &mut rsgc::collection::Visitor) {}
}

impl<'gc> State<'gc> {
    pub fn new(mc: &'gc Mutation<'gc>) -> Self {
        Self {
            dynamic_state: DynamicState::new(mc),
            vm_state: VMState::new(),
            globals: HashTable::new(mc, HashTableType::Eq, 128, 0.75),
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
        self.mutator.mutate(|mc, state| f(state.context(mc)))
    }

    pub fn new() -> Self {
        Self {
            mutator: Mutator::new(|mc| {
                super::init(mc);
                State::new(mc)
            }),
        }
    }
}
