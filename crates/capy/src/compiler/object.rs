use std::path::PathBuf;

use crate::compiler::LoweredProgram;
use crate::compiler::bytecode::{self, Constant};
use crate::compiler::cfg::CodeId;
use crate::runtime::fasl::{CodeSpec, Compression, ConstSpec, Image, ProgramSpec, Writer};
use crate::runtime::vm::thunks::make_io_error;
use crate::runtime::{
    MutationContext,
    stats::{CompilationBreakdownPhase, CompilationBreakdownScope},
    value::{Str, Symbol, Value},
};
use crate::utils::flags;

#[derive(Clone, Debug, Default)]
pub struct BackendDumpOptions {
    /// Where to write the bytecode disassembly, if anywhere.
    pub bytecode: Option<PathBuf>,
}

#[derive(Clone, Debug, Default)]
pub struct CompilationOptions {
    pub backtraces: bool,
    pub backend_dumps: BackendDumpOptions,
}

/// Procedure metadata as stored in a code block: the procedure's own
/// metadata alist with its source location added.
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

pub(crate) fn compile_lowered_to_fasl_bytes<'gc>(
    ctx: MutationContext<'gc>,
    lowered: LoweredProgram<'gc>,
    opts: CompilationOptions,
) -> Result<Vec<u8>, Value<'gc>> {
    let _stats = CompilationBreakdownScope::new(CompilationBreakdownPhase::Codegen);
    let io_error = |err: String| {
        make_io_error(
            ctx,
            "compile",
            Str::new(ctx, format!("Cannot compile unified FASL: {err}"), true).into(),
            &[],
        )
    };

    let program = bytecode::compile_program(&lowered.ssa).map_err(&io_error)?;

    if let Some(path) = &opts.backend_dumps.bytecode {
        let mut text = String::new();
        for procedure in &program.procedures {
            text.push_str(&format!(
                ";; {} ({:?}) arity {} frame {}\n",
                procedure.name, procedure.code_id, procedure.arity, procedure.frame_size
            ));
            text.push_str(&bytecode::disasm::disassemble(
                &procedure.code,
                &procedure.pool,
            ));
            text.push('\n');
        }
        std::fs::write(path, text).map_err(|err| io_error(err.to_string()))?;
    }

    let index_of = |code: CodeId| -> u32 {
        program
            .procedures
            .iter()
            .position(|procedure| procedure.code_id == code)
            .unwrap_or_else(|| panic!("closure of {code:?}, which is not in the program"))
            as u32
    };
    // Cache cells are numbered across the whole program: procedures that
    // name the same key share one cell.
    let mut cell_ids: std::collections::HashMap<u64, u32> = std::collections::HashMap::new();
    let constants: Vec<Vec<ConstSpec<'gc>>> = program
        .procedures
        .iter()
        .map(|procedure| {
            procedure
                .constants
                .iter()
                .map(|constant| match constant {
                    Constant::Value(value) => ConstSpec::Value(*value),
                    Constant::Code(code) => ConstSpec::Code(index_of(*code)),
                    Constant::Cell(key) => {
                        let next = cell_ids.len() as u32;
                        ConstSpec::Cell(*cell_ids.entry(key.bits()).or_insert(next))
                    }
                })
                .collect()
        })
        .collect();
    // `--strip-spans` drops source maps (and their span tables) at
    // serialization, lowering still builds them for dumps.
    let stripped = flags::strip_spans();
    let code_blocks: Vec<CodeSpec<'_, 'gc>> = program
        .procedures
        .iter()
        .zip(constants.iter())
        .map(|(procedure, constants)| CodeSpec {
            code: &procedure.code,
            pool: &procedure.pool,
            metadata_counts: &procedure.metadata_counts,
            constants,
            arity: procedure.arity,
            frame_size: procedure.frame_size,
            metadata: procedure_metadata(ctx, procedure.meta, procedure.source),
            spans: if stripped { &[] } else { &procedure.spans },
            srcmap: if stripped { &[] } else { &procedure.srcmap },
        })
        .collect();
    let spec = ProgramSpec {
        code_blocks: &code_blocks,
        entry: program.entry as u32,
    };

    let mut bytes = Vec::new();
    Writer::new(ctx, &mut bytes)
        .write_image(Image::Program(&spec), Compression::Gzip)
        .map_err(|err| io_error(err.to_string()))?;
    Ok(bytes)
}
