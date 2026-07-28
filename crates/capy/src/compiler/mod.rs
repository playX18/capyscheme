//! Native code compilation pipeline: CPS → CFG → Cranelift → machine code.

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
pub mod cfg;
pub mod codegen;
pub mod cps;
pub mod cranelift;
pub mod debuginfo;
pub mod direct;
mod dump;
mod object;
mod pipeline;
mod symbols;
pub mod tree;

pub use bootstrap::compile_file;
pub use object::{BackendDumpOptions, CompilationOptions};
pub use pipeline::lower_to_cps;

pub(crate) use dump::{
    begin_compilation_artifact, merge_compile_dump_options, resolve_artifact_dump_path,
};
pub(crate) use object::compile_lowered_to_fasl_bytes;
pub(crate) use pipeline::{
    DumpArtifactsOptions, LoweredProgram, dump_lowered_program_artifacts, lower_expanded_to_cps,
};
