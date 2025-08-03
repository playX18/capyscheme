use capy::cps::sexp::{CPSDeserializer, CPSSerializer};
use capy::expander::core::{Cenv, expand, seq};
use capy::frontend::reader::TreeSitter;
use capy::runtime::{Scheme, value::*};
use pretty::BoxAllocator;
use rsgc::GarbageCollector;
use rsgc::alloc::Array;
use std::io::Write;

fn main() {
    let scm = Scheme::new();

    scm.enter(|ctx| {
        let result = capy::runtime::vm::require::load(ctx, "main.scm").unwrap();
        println!("Loaded Scheme program: {}", result);
    });

    scm.mutator.collect_garbage();
}
