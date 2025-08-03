use std::{collections::HashMap, sync::OnceLock};

use rsgc::{Global, Rootable};

use super::*;
use crate::runtime::{Context, value::*};
pub type PrimitiveProc = for<'gc> extern "C-unwind" fn(ctx: Context<'gc>) -> VMReturn<'gc>;

pub extern "C-unwind" fn cons<'gc>(ctx: Context<'gc>) -> VMReturn<'gc> {
    let vm = ctx.vm();

    if vm.argument_count() != 2 {
        todo!()
    }

    let args = vm.arguments();

    VMReturn::ok(Value::cons(ctx, args[0], args[1]))
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

    pub fn get(ctx: Context<'gc>, name: Value<'gc>) -> Option<PrimitiveProc> {
        let locations = LOCATIONS
            .get_or_init(|| Global::new(PrimitiveLocations::new(ctx)))
            .fetch(&ctx);
        locations.map.get(&name).copied()
    }
}

unsafe impl<'gc> Trace for PrimitiveLocations<'gc> {
    unsafe fn process_weak_refs(&mut self, _weak_processor: &mut rsgc::WeakProcessor) {}

    unsafe fn trace(&mut self, visitor: &mut rsgc::collection::Visitor) {
        for (key, _) in &mut self.map {
            let key = key as *const Value<'gc> as *mut Value<'gc>;
            unsafe { (*key).trace(visitor) };
        }
    }
}

static LOCATIONS: OnceLock<Global<Rootable!(PrimitiveLocations<'_>)>> = OnceLock::new();
