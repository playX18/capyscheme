use std::collections::HashMap;

use crate::runtime::{Context, value::*};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum PrimitiveResult {
    Error = 0,
    Return = 1,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PrimitiveReturn<'gc> {
    pub result: PrimitiveResult,
    pub value: Value<'gc>,
}

impl<'gc> PrimitiveReturn<'gc> {
    pub const fn new(result: PrimitiveResult, value: Value<'gc>) -> Self {
        Self { result, value }
    }

    pub fn is_error(&self) -> bool {
        self.result == PrimitiveResult::Error
    }

    pub fn is_return(&self) -> bool {
        self.result == PrimitiveResult::Return
    }

    pub const fn ok(value: Value<'gc>) -> Self {
        Self::new(PrimitiveResult::Return, value)
    }

    pub const fn error(value: Value<'gc>) -> Self {
        Self::new(PrimitiveResult::Error, value)
    }
}

pub type PrimitiveProc = for<'gc> extern "C-unwind" fn(ctx: Context<'gc>) -> PrimitiveReturn<'gc>;

pub extern "C-unwind" fn cons<'gc>(ctx: Context<'gc>) -> PrimitiveReturn<'gc> {
    let vm = ctx.vm();

    if vm.argument_count() != 2 {
        todo!()
    }

    let args = vm.arguments();

    PrimitiveReturn::ok(Value::cons(ctx, args[0], args[1]))
}

pub struct PrimitiveLocations<'gc> {
    pub map: HashMap<Value<'gc>, PrimitiveProc>,
}

impl<'gc> PrimitiveLocations<'gc> {
    pub fn new(ctx: Context<'gc>) -> Self {
        let mut map: HashMap<Value<'gc>, PrimitiveProc> = HashMap::new();

        macro_rules! register_primitive {
            ($($name: literal => $prim: ident),* $(,)?) => {
                $(
                    map.insert(Symbol::from_str(ctx, $name).into(), $prim);
                )*
            };
        }

        register_primitive!(
            "cons" => cons
        );

        Self { map }
    }
}
