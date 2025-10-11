use capy::{
    init_scheme,
    runtime::{
        modules::public_ref,
        vm::{base::get_program_arguments_fluid, call_scheme},
    },
};

fn main() {
    env_logger::init();
    let scm = init_scheme();

    scm.enter(|ctx| {
        let entry = public_ref(ctx, "boot cli", "enter");

        let args = get_program_arguments_fluid(ctx);
        call_scheme(ctx, entry.expect("malformed installation"), [args]);
    });
}
