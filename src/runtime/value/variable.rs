use rsgc::{
    Collect, Gc,
    barrier::{Write, field},
    context::Mutation,
};

use super::{Tagged, TypeCode8, Value};

#[derive(Collect)]
#[collect(no_drop)]
pub struct Variable<'gc> {
    value: Value<'gc>,
}

impl<'gc> Variable<'gc> {
    pub fn new(mc: &Mutation<'gc>, value: Value<'gc>) -> Gc<'gc, Self> {
        Gc::new(mc, Self { value })
    }

    pub fn get(&self) -> Value<'gc> {
        self.value
    }

    pub fn undefined(mc: &Mutation<'gc>) -> Gc<'gc, Self> {
        Self::new(mc, Value::undefined())
    }

    pub fn set(self: &Write<Self>, value: Value<'gc>) {
        field!(self, Self, value).write(value);
    }

    pub fn unset(self: &Write<Self>) {
        field!(self, Self, value).write(Value::undefined());
    }

    pub fn is_bound(self: &Write<Self>) -> bool {
        self.value != Value::undefined()
    }
}

unsafe impl<'gc> Tagged for Variable<'gc> {
    const TC8: TypeCode8 = TypeCode8::VARIABLE;
}
