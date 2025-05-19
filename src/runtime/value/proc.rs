use std::task::Context;

use crate::runtime::errors::Violation;

use super::Value;


pub enum Callback<'gc> {
    Primitive(fn(Context<'gc>, &[Value<'gc>]) -> Result<Value<'gc>, Violation<'gc>>),
    Boxed(Box<dyn Fn(Context<'gc>, &[Value<'gc>]) -> Result<Value<'gc>, Violation<'gc>> + 'gc>),
}