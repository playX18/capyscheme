use capy::expander::library::{import_to_filename, read_import};
use capy::runtime::modules::mangle_library_spec;
use capy::runtime::{Context, Scheme, value::*};
use capy::{expander::*, list};

fn main() {
    let scm = Scheme::new();

    scm.enter(|ctx| {
        let spec = list!(ctx, Symbol::from_str(ctx, "foo"));
        let spec = list!(
            ctx,
            Symbol::from_str(ctx, "prefix"),
            spec,
            Symbol::from_str(ctx, "foo:")
        );
        let spec = list!(
            ctx,
            Symbol::from_str(ctx, "only"),
            spec,
            Symbol::from_str(ctx, "bar")
        );

        let import = read_import(ctx, spec).unwrap();

        println!("Import: {import}");
    });
}
