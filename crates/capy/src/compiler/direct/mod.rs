//! The seed emitter: Tree IL straight to bytecode.
//!
//! This is the compiler stage-0 uses before the Scheme compiler exists. It does no CPS, builds no
//! CFG and optimizes nothing, and it runs no Tree IL pass: it takes the raw output of the Rust
//! expander (or of the Scheme front passes) and emits bytecode term by term.
//!
//! - Every variable lives in a frame slot. Slots are handed out like a stack, and a slot is in
//!   the liveness mask of a return point only while it holds a value.
//! - Closures are flat. A closure copies the value of each free variable, and the callee loads
//!   them into slots on entry.
//! - A variable that is `set!` or bound by `letrec` lives in a box (a `Variable`), and the slot
//!   holds the box.
//! - A call evaluates the callee and the arguments straight into the callee frame, above all the
//!   live slots, like `bytecode::lower` lays out a call.
//! - Primitive calls use the generic `call_primitive*` instructions. A primitive that has no
//!   direct form, or that is called with the wrong count, becomes a call of its procedure.
//! - `with-continuation-mark` calls the native `call-with-continuation-mark` with a body thunk.
//! - `case-lambda` is a variadic procedure that dispatches on the length of its arguments.
//!
//! The result is a [`DirectProgram`], written with the FASL writer and a directory header.

mod analysis;
mod emit;

use crate::bytecode::def::{self as bc};
use crate::bytecode::disasm;
use crate::expander::core::{LinkletTerm, TermKind, TermRef};
use crate::runtime::fasl::{CodeSpec, Compression, ConstSpec, Image, ProgramSpec, SrcRun, Writer};
use crate::runtime::vm::thunks::make_io_error;
use crate::runtime::{
    MutationContext,
    objectmodel::{Str, Symbol, Value},
};
use crate::utils::flags;
use std::path::PathBuf;

/// One entry of a constant pool.
#[derive(Clone, Copy, Debug)]
pub(crate) enum DirectConst<'gc> {
    Value(Value<'gc>),
    /// The code block with this index in the program.
    Code(u32),
    /// The cache cell of this key.
    Cell(Value<'gc>),
}

/// One finished code block.
#[derive(Clone)]
pub(crate) struct DirectCode<'gc> {
    pub code: Vec<u8>,
    pub pool: Vec<u32>,
    pub metadata_counts: [u32; bc::NUM_METADATA_KINDS],
    pub constants: Vec<DirectConst<'gc>>,
    pub arity: i32,
    pub frame_size: u32,
    pub meta: Value<'gc>,
    pub source: Value<'gc>,
    pub name: Value<'gc>,
    pub srcmap: Vec<SrcRun>,
    pub spans: Vec<Value<'gc>>,
}

#[derive(Clone)]
pub(crate) struct DirectProgram<'gc> {
    pub blocks: Vec<DirectCode<'gc>>,
    pub entry: u32,
    /// Keeps what the compiler allocated alive until the program is written.
    pub keep: Option<crate::heap::Gc<'gc, emit::Keep<'gc>>>,
}

/// A compiled program with the Tree IL it came from, whose linklets give the directory header.
#[derive(Clone)]
pub(crate) struct SeedProgram<'gc> {
    pub(crate) original_il: TermRef<'gc>,
    pub(crate) direct: DirectProgram<'gc>,
}

/// Compile `il`, the raw output of the Rust expander, to a program.
pub(crate) fn lower_term<'gc>(
    ctx: MutationContext<'gc>,
    il: TermRef<'gc>,
) -> Result<SeedProgram<'gc>, Value<'gc>> {
    let original_il = il;
    // Raw expander output still has primitive calls to resolve and letrec to rewrite.
    let il = {
        use crate::expander::{primitives, recursive_bindings, well_known_procs};
        let il = primitives::resolve_primitive_refs(ctx, il);
        let il = primitives::expand_primitive_calls(ctx, il);
        let il = recursive_bindings::rewrite_recursive_bindings(ctx, il);
        well_known_procs::expand_well_known_procs(ctx, il)
    };
    let program = emit::compile_program(ctx, il).map_err(|err| {
        crate::runtime::vm::exceptions::make_assertion_violation(
            ctx,
            Some("seed-compile"),
            &format!("compiler error: {err}"),
            &[],
        )
    })?;
    Ok(SeedProgram {
        original_il,
        direct: program,
    })
}

/// The directory header of a top-level term: one `(name imports exports meta)` entry per
/// linklet, for a linklet or a `make-linklet-directory` over linklets. Other terms have none.
fn directory_header<'gc>(ctx: MutationContext<'gc>, il: TermRef<'gc>) -> Option<Value<'gc>> {
    let entry = |linklet: &LinkletTerm<'gc>| {
        Value::list_from_slice(
            ctx,
            [linklet.name, linklet.imports, linklet.exports, linklet.meta],
        )
    };
    match il.kind {
        TermKind::Linklet(linklet) => Some(Value::list_from_slice(ctx, [entry(&linklet)])),
        TermKind::Call(op, args)
            if matches!(op.kind, TermKind::PrimRef(name)
                if name == Symbol::from_str(ctx, "make-linklet-directory").into()) =>
        {
            let mut entries = Vec::with_capacity(args.len());
            for arg in args.iter() {
                match arg.kind {
                    TermKind::Linklet(linklet) => entries.push(entry(&linklet)),
                    _ => return None,
                }
            }
            Some(Value::list_from_slice(ctx, entries))
        }
        _ => None,
    }
}

/// Procedure metadata as stored in a code block: the procedure's own metadata alist with its
/// source location added.
fn procedure_metadata<'gc>(
    ctx: MutationContext<'gc>,
    metadata: Value<'gc>,
    source: Value<'gc>,
) -> Value<'gc> {
    if source == Value::new(false) {
        return metadata;
    }
    let source_key = Symbol::from_str(ctx, "source").into();
    if metadata.is_pair() && metadata.assq(source_key).is_some() {
        return metadata;
    }
    let metadata = if metadata == Value::new(false) {
        Value::null()
    } else {
        metadata
    };
    if !metadata.is_alist() {
        return metadata;
    }
    Value::cons(ctx, Value::cons(ctx, source_key, source), metadata)
}

/// The FASL image of the program of `lowered`.
pub(crate) fn write_fasl<'gc>(
    ctx: MutationContext<'gc>,
    lowered: &SeedProgram<'gc>,
) -> Result<Vec<u8>, Value<'gc>> {
    let program = &lowered.direct;
    let io_error = |err: String| {
        make_io_error(
            ctx,
            "compile",
            Str::new(ctx, format!("Cannot compile unified FASL: {err}"), true).into(),
            &[],
        )
    };

    // Cache cells are numbered across the whole program, in first-seen order.
    let mut cell_ids: std::collections::HashMap<u64, u32> = std::collections::HashMap::new();
    let constants: Vec<Vec<ConstSpec<'gc>>> = program
        .blocks
        .iter()
        .map(|block| {
            block
                .constants
                .iter()
                .map(|constant| match constant {
                    DirectConst::Value(value) => ConstSpec::Value(*value),
                    DirectConst::Code(index) => ConstSpec::Code(*index),
                    DirectConst::Cell(key) => {
                        let next = cell_ids.len() as u32;
                        ConstSpec::Cell(*cell_ids.entry(key.bits()).or_insert(next))
                    }
                })
                .collect()
        })
        .collect();
    let stripped = flags::strip_spans();
    let code_blocks: Vec<CodeSpec<'_, 'gc>> = program
        .blocks
        .iter()
        .zip(constants.iter())
        .map(|(block, constants)| CodeSpec {
            code: &block.code,
            pool: &block.pool,
            metadata_counts: &block.metadata_counts,
            constants,
            arity: block.arity,
            frame_size: block.frame_size,
            metadata: procedure_metadata(ctx, block.meta, block.source),
            spans: if stripped { &[] } else { &block.spans },
            srcmap: if stripped { &[] } else { &block.srcmap },
        })
        .collect();
    let spec = ProgramSpec {
        code_blocks: &code_blocks,
        entry: program.entry,
    };

    let mut bytes = Vec::new();
    Writer::new(ctx, &mut bytes)
        .with_directory(directory_header(ctx, lowered.original_il))
        .write_image(Image::Program(&spec), Compression::Gzip)
        .map_err(|err| io_error(err.to_string()))?;
    Ok(bytes)
}
