use capy::runtime::{Scheme, vm::base::get_program_arguments_fluid};

fn main() {
    env_logger::init();
    let scm = Scheme::new();

    /*scm.collect_garbage();
    scm.enter(|ctx| {
        let builder = ImageBuilder::new(ctx);

        let code = builder.serialize().expect("Failed to serialize image");

        std::fs::write("capy_image.bin", &code).expect("Failed to write image file");
    });*/
    scm.call(
        "boot cli",
        "enter",
        |ctx, args| {
            let cli = get_program_arguments_fluid(*ctx);
            args.push(cli);
        },
        |_, _| (),
    );
}
