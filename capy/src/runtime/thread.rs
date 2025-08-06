use rsgc::{Gc, Mutation, Mutator, Rootable, Trace, Write};

use crate::{
    //jit::trampoline::TRAMPOLINES,
    runtime::{
        fluids::DynamicState,
        value::{
            Closure, HashTable, HashTableType, Str,
            Value, /*  Variable, current_environment, current_variable_environment*/
        },
        //vm::{VMReturnCode, VMState, exit_continuation},
    },
};

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Context<'gc> {
    mc: &'gc Mutation<'gc>,
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
    }

    pub fn call(
        self,
        proc: Value<'gc>,
        args: impl AsRef<[Value<'gc>]>,
    ) -> Result<Value<'gc>, Value<'gc>> {
        if !proc.is::<Closure>() {
            return Err(proc);
        }

        let closure = proc.downcast::<Closure>();
        let entrypoint = closure.code;
        if closure.is_continuation() {
            return Err(Value::new(Str::new(
                &self,
                "Non-procedure call".to_string(),
                false,
            )));
        }
        unsafe {
            Write::assume(&self.vm().proc).write(proc);
            for &arg in args.as_ref().iter() {
                self.vm().push_arg(arg);
            }
        }

        let k = exit_continuation(self);
        assert!(k.is::<Closure>());
        let trampoline = TRAMPOLINES.rust_to_scheme();

        let result = trampoline(&self.mc, &self.state, entrypoint, exit_continuation(self));

        match result.code {
            VMReturnCode::Return => Ok(result.value),
            VMReturnCode::Error => Err(result.value),
            VMReturnCode::NonProcedureCall => Err(Value::new(Str::new(
                &self,
                "Non-procedure call".to_string(),
                false,
            ))),
            VMReturnCode::ArgumentMismatch => Err(Value::new(Str::new(
                &self,
                "Argument mismatch".to_string(),
                false,
            ))),
            VMReturnCode::Throw => Err(result.value),
            VMReturnCode::ThrowValue => Err(result.value),
            VMReturnCode::Yield => {
                println!("yield requested? {}", self.mc.take_yieldpoint());
                Ok(result.value)
            }
            VMReturnCode::ThrowValueAndData => Err(result.value),
        }
    }

    pub fn vm(&self) -> &VMState<'gc> {
        &self.state.vm_state
    }*/

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
    //pub(crate) vm_state: VMState<'gc>,
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
