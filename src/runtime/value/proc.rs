use rsgc::Trace;

use super::super::Context;
use super::Value;

pub enum Callback<'gc> {
    Primitive(fn(Context<'gc>, &[Value<'gc>]) -> Result<Value<'gc>, Value<'gc>>),
    Boxed(Box<dyn Fn(Context<'gc>, &[Value<'gc>]) -> Result<Value<'gc>, Value<'gc>> + 'gc>),

    Procedure(Value<'gc>),
}

unsafe impl<'gc> Trace for Callback<'gc> {
    fn trace(&mut self, visitor: &mut rsgc::Visitor<'_>) {
        match self {
            Self::Procedure(x) => x.trace(visitor),
            _ => (),
        }
    }
}

unsafe impl<'gc> rsgc::EnsureGCInfo<'gc> for Callback<'gc> {}
