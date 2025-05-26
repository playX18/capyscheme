use std::{hash::Hash, sync::OnceLock};

use capyscheme::runtime::{Scheme, value::*};
use rsgc::{Gc, Rootable, mutator::Global};
use simplehash::MurmurHasher64;

static X: OnceLock<Global<Rootable!(Gc<'_, WeakMapping<'_>>)>> = OnceLock::new();

fn main() {
    env_logger::init();
    let scm = Scheme::new();

    scm.enter(|ctx| {
        let p = Pair::new(ctx, Value::new(1), Value::new(2));
        let wmap = WeakMapping::new(ctx, p.into(), Value::new(3));
        println!("wmap at {:p}", wmap);
        X.get_or_init(|| Global::new(wmap));
        let mut hasher = MurmurHasher64::new(5382);
        Value::new(wmap).hash(&mut hasher);
        println!("hash {:x}", hasher.finish_u64());
    });

    for _ in 0..4 {
        scm.mutator.request_gc();

        scm.enter(|ctx| {
            let x = X.get().unwrap().fetch(&ctx);
            println!("wmap at {:p}", *x);
            println!("{:x}", x.key().raw_i64());
            let mut hasher = MurmurHasher64::new(5382);
            Value::new(*x).hash(&mut hasher);
            println!("hash {:x}", hasher.finish_u64());
        });
    }
}
