/*use std::sync::OnceLock;

use rsgc::{Gc, Global, Rootable, barrier, cell::Lock};

use crate::{
    fluid,
    runtime::{Context, value::*},
};

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Environment<'gc> {
    pub hdr: ScmHeader,
    pub variables: Lock<Gc<'gc, HashTable<'gc>>>,
    pub macros: Lock<Gc<'gc, HashTable<'gc>>>,
    pub name: Value<'gc>,
}

unsafe impl<'gc> Tagged for Environment<'gc> {
    const TC8: TypeCode8 = TypeCode8::ENVIRONMENT;
}

pub type EnvironmentRef<'gc> = Gc<'gc, Environment<'gc>>;

fluid! {
    pub thread_local current_dynamic_environment = Value::null();

}

static CURRENT_ENVIRONMENT: OnceLock<Global<Rootable!(EnvironmentRef<'_>)>> = OnceLock::new();

pub fn current_environment<'gc>(ctx: Context<'gc>) -> EnvironmentRef<'gc> {
    *CURRENT_ENVIRONMENT
        .get_or_init(|| {
            let env = Gc::new(
                &ctx,
                Environment {
                    hdr: ScmHeader::with_type_bits(TypeCode8::ENVIRONMENT.bits() as u16),
                    variables: Lock::new(HashTable::new(&ctx, HashTableType::Eq, 32, 0.75)),
                    macros: Lock::new(HashTable::new(&ctx, HashTableType::Eq, 32, 0.75)),
                    name: Value::undefined(),
                },
            );
            Global::new(env)
        })
        .fetch(&ctx)
}

pub fn current_variable_environment<'gc>(ctx: Context<'gc>) -> Gc<'gc, HashTable<'gc>> {
    current_environment(ctx).variables.get()
}

pub fn set_current_variable_environment<'gc>(
    ctx: Context<'gc>,
    variables: Gc<'gc, HashTable<'gc>>,
) {
    let env = current_environment(ctx);

    let env = Gc::write(&ctx, env);
    barrier::field!(env, Environment, variables)
        .unlock()
        .set(variables);
}

pub fn current_macro_environment<'gc>(ctx: Context<'gc>) -> Gc<'gc, HashTable<'gc>> {
    current_environment(ctx).macros.get()
}

pub fn set_current_macro_environment<'gc>(ctx: Context<'gc>, macros: Gc<'gc, HashTable<'gc>>) {
    let env = current_environment(ctx);

    let env = Gc::write(&ctx, env);
    barrier::field!(env, Environment, macros)
        .unlock()
        .set(macros);
}

#[derive(Trace)]
#[collect(no_drop)]
#[repr(C)]
pub struct Variable<'gc> {
    pub hdr: ScmHeader,
    pub value: Value<'gc>,
}

impl<'gc> Variable<'gc> {
    pub fn new(ctx: Context<'gc>, value: Value<'gc>) -> Gc<'gc, Self> {
        Gc::new(
            &ctx,
            Self {
                hdr: ScmHeader::with_type_bits(TypeCode8::VARIABLE.bits() as u16),
                value,
            },
        )
    }

    pub fn get(&self) -> Value<'gc> {
        self.value
    }

    pub fn set(self: Gc<'gc, Self>, ctx: Context<'gc>, value: Value<'gc>) {
        let this = Gc::write(&ctx, self);
        barrier::field!(this, Variable, value).write(value);
    }
}

unsafe impl<'gc> Tagged for Variable<'gc> {
    const TC8: TypeCode8 = TypeCode8::VARIABLE;
}
*/
