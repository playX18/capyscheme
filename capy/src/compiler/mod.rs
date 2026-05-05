//! Native code compilation pipeline: CPS → SSA → Cranelift → machine code.

#[macro_export]
macro_rules! call_signature {
    ($callconv: ident ($($arg: ident),*) -> $ret: ident) => {
        {
            let mut sig = ir::Signature::new(CallConv::$callconv);
            {
                $(
                    sig.params.push(ir::AbiParam::new(ir::types::$arg));
                )*
                sig.returns.push(ir::AbiParam::new(ir::types::$ret));
            }
            sig
        }
    };

    ($callconv: ident ($($arg: ident),*) -> ($($ret:ident),*)) => {
        {
            let mut sig = ir::Signature::new(CallConv::$callconv);
            {
                $(
                    sig.params.push(ir::AbiParam::new(ir::types::$arg));
                )*
                $(
                    sig.returns.push(ir::AbiParam::new(ir::types::$ret));
                )*
            }

            sig
        }
    };

    ($callconv: ident ($($arg: ident),*)) => {
        {
            let mut sig = ir::Signature::new(CallConv::$callconv);
            {
                $(
                    sig.params.push(ir::AbiParam::new(ir::types::$arg));
                )*
            }

            sig
        }
    }
}

mod artifact;
mod bootstrap;
pub mod bytecode;
pub mod debuginfo;
pub mod linkutils;
pub mod module_graph;
mod object;
mod pipeline;
pub mod ssa;

pub use bootstrap::compile_file;
pub use object::{CompilationOptions, compile_cps_to_object, link_object_product};
pub use pipeline::lower_to_cps;

pub(crate) use object::{compile_cps_to_shared_object, compile_cps_to_static_module_object};
pub(crate) use pipeline::{
    DumpArtifactsOptions, LoweredProgram, dump_lowered_program_artifacts, lower_expanded_to_cps,
};
