use super::*;
use crate::runtime::Context;
use rsgc::Gc;

pub unsafe trait Tagged {
    const TC8: TypeCode8;
    const TC16: &[TypeCode16] = &[];

    /// Set to true when the type can only be encodded as a TC16. In that case
    /// `Value::is` and `Value::downcast` will always check `TC16` and not `TC8`.
    const ONLY_TC16: bool = false;
}

pub trait IntoValue<'gc> {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc>;
}

pub trait FromValue<'gc>: Sized {
    fn try_from_value(ctx: Context<'gc>, value: Value<'gc>) -> Result<Self, Value<'gc>>;
}

impl<'gc> IntoValue<'gc> for Value<'gc> {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        self
    }
}

impl<'gc> Into<Value<'gc>> for i32 {
    fn into(self) -> Value<'gc> {
        Value::from_i32(self)
    }
}

impl<'gc> Into<Value<'gc>> for f64 {
    fn into(self) -> Value<'gc> {
        Value::from_f64(self)
    }
}

impl<'gc> Into<Value<'gc>> for bool {
    fn into(self) -> Value<'gc> {
        Value::from_bool(self)
    }
}

impl<'gc> Into<Value<'gc>> for () {
    fn into(self) -> Value<'gc> {
        Value::null()
    }
}

impl<'gc> Into<Value<'gc>> for char {
    fn into(self) -> Value<'gc> {
        Value::from_char(self)
    }
}

impl<'gc, T: Tagged> IntoValue<'gc> for Gc<'gc, T> {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::from_gc(self)
    }
}

impl<'gc> IntoValue<'gc> for i32 {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::from_i32(self)
    }
}

impl<'gc> IntoValue<'gc> for f64 {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::from_f64(self)
    }
}

impl<'gc> IntoValue<'gc> for bool {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::from_bool(self)
    }
}

impl<'gc> IntoValue<'gc> for () {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::null()
    }
}

impl<'gc> IntoValue<'gc> for &str {
    fn into_value(self, mc: Context<'gc>) -> Value<'gc> {
        let string = Str::new(&mc, self, false);

        Value::from_gc(string)
    }
}

impl<'gc> IntoValue<'gc> for char {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Value::from_char(self)
    }
}

impl<'gc> IntoValue<'gc> for usize {
    fn into_value(self, _mc: Context<'gc>) -> Value<'gc> {
        Number::from_usize(_mc, self).into_value(_mc)
    }
}
