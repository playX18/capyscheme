use capy::{
    init_scheme,
    runtime::{
        modules::public_ref,
        vm::{VMResult, base::get_program_arguments_fluid, call_scheme},
    },
};

fn main() {
    env_logger::init();
    let scm = init_scheme();

    let mut did_yield = scm.enter(|ctx| {
        let entry = public_ref(ctx, "boot cli", "enter");

        let args = get_program_arguments_fluid(ctx);
        match call_scheme(ctx, entry.expect("malformed installation"), [args]) {
            VMResult::Yield => true,
            _ => false,
        }
    });

    while did_yield {
        scm.enter(|ctx| {
            if ctx.has_suspended_call() {
                match ctx.resume_suspended_call() {
                    VMResult::Yield => did_yield = true,
                    _ => did_yield = false,
                }
            } else {
                did_yield = false;
            }
        })
    }
}
