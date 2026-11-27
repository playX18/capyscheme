//! Reading compiled library images.
//!
//! Nothing here keeps what it reads alive. The entry thunk of an image is rooted by whoever
//! calls it, and every procedure it defines holds its own `CodeBlock`, which owns the bytecode
//! and the machine code of that procedure.

use std::{fs, io::Cursor};

use crate::runtime::{
    MutationContext,
    fasl::{Reader, fasl_payload},
    objectmodel::Value,
    vm::load::{
        artifact::{LoadArtifact, LoadArtifactKind},
        policy::get_fasl_load_options,
    },
};

/// Reads the compiled `artifact` and returns its entry thunk.
pub(crate) fn load_artifact<'gc>(
    ctx: MutationContext<'gc>,
    artifact: &LoadArtifact,
) -> std::io::Result<Value<'gc>> {
    match artifact.kind {
        LoadArtifactKind::SharedObject => Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "shared-object Scheme artifacts are no longer loadable",
        )),
        LoadArtifactKind::FaslCode => load_image(ctx, &fs::read(&artifact.path)?),
    }
}

/// Reads a FASL code image that is in memory and returns its entry thunk.
pub(crate) fn load_image<'gc>(
    ctx: MutationContext<'gc>,
    bytes: &[u8],
) -> std::io::Result<Value<'gc>> {
    Reader::new_with_options(
        ctx,
        Cursor::new(fasl_payload(bytes)),
        get_fasl_load_options(),
    )
    .read()
}
