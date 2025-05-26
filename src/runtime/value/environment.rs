use rsgc::{Collect, Gc};

use crate::runtime::Context;

use super::*;

#[derive(Collect)]
#[collect(no_drop)]
pub struct Environment<'gc> {
    pub variable: Gc<'gc, HashTable<'gc>>,
    pub macro_: Gc<'gc, HashTable<'gc>>,
    pub name: Gc<'gc, String<'gc>>,
}

impl<'gc> Environment<'gc> {
    pub fn new(ctx: Context<'gc>, name: &str) -> Gc<'gc, Self> {
        let variable = HashTable::new(ctx, HashTableType::Eq, 11, 0.75);
        let macro_ = HashTable::new(ctx, HashTableType::Eq, 11, 0.75);
        let name = String::new(&ctx, name, false);
        Gc::new(
            &ctx,
            Self {
                variable,
                macro_,
                name,
            },
        )
    }
}
