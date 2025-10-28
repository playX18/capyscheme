#![allow(
    clippy::type_complexity,
    internal_features,
    clippy::new_without_default,
    clippy::from_over_into,
    clippy::not_unsafe_ptr_arg_deref
)]
#![feature(
    arbitrary_self_types,
    try_trait_v2,
    link_llvm_intrinsics,
    min_specialization,
    portable_simd
)]
//pub mod compiler;
pub mod cps;
pub mod expander;
pub mod frontend;
//pub mod jit;
pub mod compiler;
pub mod runtime;
pub mod utils;

pub use rsgc;
use rsgc::{MMTKBuilder, mmtk::util::options::PlanSelector};

use crate::runtime::{
    Scheme,
    modules::{current_module, root_module},
    vm::{VMResult, call_scheme, load::load_thunk_in_vicinity},
};

pub fn init_scheme() -> Scheme {
    let mut mmtk_builder = MMTKBuilder::new();
    /*if (*mmtk_builder.options.plan) == PlanSelector::GenCopy {
        mmtk_builder.options.plan.set(PlanSelector::SemiSpace);
    }

    if matches!(
        *mmtk_builder.options.plan,
        PlanSelector::GenImmix | PlanSelector::StickyImmix
    ) {
        mmtk_builder.options.plan.set(PlanSelector::Immix);
    }*/

    if (*mmtk_builder.options.plan) == PlanSelector::MarkCompact {
        println!(";; Warning: MarkCompact is not supported, using MarkSweep instead.");
        mmtk_builder.options.plan.set(PlanSelector::MarkSweep);
    }

    let scm = Scheme::new();

    let mut did_yield = scm.enter(|ctx| {
        current_module(ctx).set(ctx, (*root_module(ctx)).into());

        let thunk =
            load_thunk_in_vicinity::<true>(ctx, "boot/main.scm", None::<&str>, false, false)
                .expect("Failed to load boot/main.scm");

        let result = call_scheme(ctx, thunk, []);

        match result {
            VMResult::Ok(_) => {}
            VMResult::Err(err) => {
                eprintln!("Failed to boot: {err}");
                std::process::exit(1);
            }
            VMResult::Yield => {
                return true;
            }
        }

        false
    });

    while did_yield {
        did_yield = scm.enter(|ctx| {
            if ctx.has_suspended_call() {
                match ctx.resume_suspended_call() {
                    VMResult::Ok(_) => false,
                    VMResult::Err(err) => {
                        eprintln!("Failed to boot: {err}");
                        std::process::exit(1);
                    }
                    VMResult::Yield => true,
                }
            } else {
                false
            }
        })
    }

    scm
}

pub mod prelude {
    pub use crate::list;
    pub use crate::runtime::modules::Module;
    pub use crate::runtime::modules::current_module;
    pub use crate::runtime::prelude::*;
    pub use crate::runtime::vm::{NativeCallContext, NativeCallReturn};
    pub use crate::vector;
    pub use capy_derive::scheme;
    pub use rsgc::{Global, Rootable, Trace, barrier};
}

pub use capy_derive::scheme;
