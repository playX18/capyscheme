use capy::runtime::{Scheme, vm::base::get_program_arguments_fluid};

fn main() {
    env_logger::init();

    let scm = Scheme::new();

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
