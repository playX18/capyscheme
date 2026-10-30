use std::path::{Path, PathBuf};

use crate::compiler::{
    BackendDumpOptions, CompilationOptions, DumpArtifactsOptions, LoweredProgram,
    begin_compilation_artifact, dump_lowered_program_artifacts, lower_expanded_to_cps,
    merge_compile_dump_options, resolve_artifact_dump_path,
};
use crate::list;
use crate::prelude::*;
use crate::runtime::value::{Closure, Str, Symbol, Value};
use crate::runtime::vm::base::scm_log_level;
use crate::runtime::vm::expand::TermConverter;
use crate::runtime::vm::libraries::LIBRARY_COLLECTION;
use crate::runtime::vm::thunks::make_io_error;
#[cfg(feature = "bootstrap")]
use crate::runtime::vm::thunks::make_lexical_violation;
use capy_derive::scheme;

use super::paths::{fallback_file_name, find_path_to};
use super::{
    artifact::LoadArtifact,
    compile::{
        CompilationPhase, ExpandedJob, Unsaved, compile_lowered_to_destination,
        destination_artifact_for_current_policy, load_thunk_in_vicinity, load_unsaved,
        take_unsaved,
    },
    policy::set_fasl_debug_entries,
};
use crate::runtime::compile_service::{self, Failure, JobInput};

type LoadResult<'gc> = Result<Value<'gc>, Value<'gc>>;
type OptionalStringRef<'gc> = Option<StringRef<'gc>>;

#[scheme(path=capy)]
pub(super) mod load_ops {
    #[allow(unused_imports)]
    use super::*;

    #[scheme(name = "%find-path-to")]
    pub fn scm_find_path_to(
        filename: StringRef<'gc>,
        resolve_relative: bool,
        in_vicinity: OptionalStringRef<'gc>,
        arch: OptionalStringRef<'gc>,
    ) -> LoadResult<'gc> {
        let filename = PathBuf::from(filename.as_gc_ref().to_string());
        let in_vicinity = in_vicinity.map(|path| PathBuf::from(path.as_gc_ref().to_string()));
        let arch = arch.map(|value| value.to_string());
        let result = find_path_to_public(
            ctx,
            filename,
            resolve_relative,
            in_vicinity,
            arch.as_deref(),
        );
        result
    }

    #[scheme(name = "%compile")]
    pub fn compile(
        expanded: Value<'gc>,
        destination: StringRef<'gc>,
        load_thunk: Option<bool>,
        dump_selections: Option<Value<'gc>>,
        preprocessed: Option<bool>,
    ) -> LoadResult<'gc> {
        let dump_options = compile_dump_options(ctx, dump_selections);
        let result = compile_expanded_to_destination(
            ctx,
            expanded,
            destination.to_string().as_str(),
            CompilationOptions {
                backtraces: compile_backtraces_enabled(ctx),
                ..CompilationOptions::default()
            },
            load_thunk.unwrap_or(true),
            dump_options,
            preprocessed.unwrap_or(false),
        );
        result
    }

    /// Compiles the source file `source` to the artifact `destination` with the Rust boot
    /// expander
    #[scheme(name = "%compile-boot-file")]
    pub fn compile_boot_file(
        source: StringRef<'gc>,
        destination: StringRef<'gc>,
    ) -> LoadResult<'gc> {
        #[cfg(feature = "bootstrap")]
        {
            let destination =
                destination_artifact_for_current_policy(destination.to_string().as_str());
            let _phase = CompilationPhase::new(ctx);
            let job = crate::runtime::vm::load::SourceJob {
                source: PathBuf::from(source.to_string()),
                destination,
                unsaved: None,
            };
            let (ctx, outcome) = compile_service::run(ctx, JobInput::Source(job));
            compile_service::raise_failure(ctx, outcome.unwrap_or_else(|killed| killed.unwind()))?;
            Ok(Value::undefined())
        }
        #[cfg(not(feature = "bootstrap"))]
        {
            let _ = (source, destination);
            Err(make_io_error(
                ctx,
                "%compile-boot-file",
                Str::new(
                    ctx,
                    "%compile-boot-file is not available in non-bootstrap builds",
                    true,
                )
                .into(),
                &[],
            ))
        }
    }

    #[scheme(name = "try-load-thunk-in-vicinity")]
    pub fn scm_try_load_thunk_in_vicinity(
        filename: StringRef<'gc>,
        resolve_relative: bool,
        in_vicinity: OptionalStringRef<'gc>,
    ) -> LoadResult<'gc> {
        let filename = PathBuf::from(filename.as_gc_ref().to_string());
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_gc_ref().to_string()));
        let result =
            load_thunk_in_vicinity::<false>(ctx, filename, in_vicinity, resolve_relative, None);
        result
    }

    #[scheme(name = "load-thunk-in-vicinity")]
    pub fn scm_load_thunk_in_vicinity(
        filename: StringRef<'gc>,
        resolve_relative: bool,
        in_vicinity: OptionalStringRef<'gc>,
    ) -> LoadResult<'gc> {
        let filename = PathBuf::from(filename.as_gc_ref().to_string());
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_gc_ref().to_string()));
        let result =
            load_thunk_in_vicinity::<true>(ctx, filename, in_vicinity, resolve_relative, None);
        result
    }

    #[scheme(name = "%bootstrapping?")]
    pub fn scm_bootstrapping() -> LoadResult<'gc> {
        #[cfg(feature = "bootstrap")]
        {
            Ok(Value::new(true))
        }
        #[cfg(not(feature = "bootstrap"))]
        {
            Ok(Value::new(false))
        }
    }

    #[scheme(name = "load-debug-enable!")]
    pub fn load_debug_enable(enabled: bool) -> Value<'gc> {
        set_fasl_debug_entries(enabled);
        Value::undefined()
    }

    #[scheme(name = "load-thunk-in-vicinity-k")]
    #[allow(dead_code)]
    #[allow(unused_variables)]
    pub fn scm_load_thunk_in_vicinity_k(
        filename: StringRef<'gc>,
        k: Value<'gc>,
        env: Value<'gc>,
        resolve_relative: bool,
        in_vicinity: OptionalStringRef<'gc>,
    ) -> LoadResult<'gc> {
        let filename = PathBuf::from(filename.as_gc_ref().to_string());
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_gc_ref().to_string()));
        let result =
            load_thunk_in_vicinity::<false>(ctx, filename, in_vicinity, resolve_relative, None);

        match result {
            Ok(thunk) if thunk.is::<Closure>() => Ok(thunk),
            Ok(thunk) => {
                #[cfg(feature = "bootstrap")]
                {
                    let file = thunk.car().downcast::<Str>().to_string();
                    let file_in = match std::fs::File::open(&file).map_err(|e| {
                        make_io_error(
                            ctx,
                            "compile-file",
                            Str::new(
                                ctx,
                                format!("Cannot open input file '{}': {}", file, e),
                                true,
                            )
                            .into(),
                            &[],
                        )
                    }) {
                        Ok(file_in) => file_in,
                        Err(err) => return Err(err),
                    };

                    let text = match std::io::read_to_string(&file_in).map_err(|e| {
                        make_io_error(
                            ctx,
                            "compile-file",
                            Str::new(
                                ctx,
                                format!("Cannot read input file '{}': {}", file, e),
                                true,
                            )
                            .into(),
                            &[],
                        )
                    }) {
                        Ok(text) => text,
                        Err(err) => return Err(err),
                    };
                    let src = Str::new(ctx, &file, true);
                    let parser =
                        crate::frontend::reader::TreeSitter::new(ctx, &text, src.into(), true);
                    let program = match parser.read_program().map_err(|err| {
                        make_lexical_violation(ctx, "compile-file", err.display_with_file(&file))
                    }) {
                        Ok(program) => program,
                        Err(err) => return Err(err),
                    };

                    let after_call =
                        make_closure_continue_loading_k(ctx, [thunk, Value::current_k()]);
                    let program = Value::list_from_slice(ctx, program);
                    call.call_with(k, &[program, env], after_call.into());
                    return Ok(Value::undefined());
                }

                #[cfg(not(feature = "bootstrap"))]
                {
                    Err(make_io_error(
                        ctx,
                        "load-thunk-in-vicinity-k",
                        Str::new(
                            ctx,
                            "load-thunk-in-vicinity-k is not available in non-bootstrap builds",
                            true,
                        )
                        .into(),
                        &[],
                    ))
                }
            }
            Err(err) => Err(err),
        }
    }

    /// The import specs of the `(|#%require| spec ...)` header of the boot file at `path`,
    /// or `#f` when the file has no such header
    #[scheme(name = "%boot-header-requires")]
    pub fn boot_header_requires(path: StringRef<'gc>) -> LoadResult<'gc> {
        let path = path.to_string();
        let text = std::fs::read_to_string(&path).map_err(|e| {
            make_io_error(
                ctx,
                "%boot-header-requires",
                Str::new(ctx, format!("Cannot read '{}': {}", path, e), true).into(),
                &[],
            )
        })?;
        Ok(super::scan_boot_requires(ctx, &text).unwrap_or(Value::new(false)))
    }

    #[scheme(name = "%search-load-path")]
    pub fn search_load_path(filename: StringRef<'gc>, arch: Option<StringRef<'gc>>) -> Value<'gc> {
        let filename = PathBuf::from(filename.as_gc_ref().to_string());
        let arch = arch.map(|value| value.to_string());
        let result = find_path_to_public(ctx, filename, true, None::<PathBuf>, arch.as_deref());
        match result {
            Ok(value) => value,
            Err(_) => Value::new(false),
        }
    }

    #[scheme(name = "stdlib-path")]
    pub fn stdlib_path() -> Value<'gc> {
        let mut load_path = ctx.globals().loc_load_path().get();
        while load_path.is_pair() {
            let base = PathBuf::from(load_path.car().downcast::<Str>().to_string());
            if base.join("boot").join("main.scm").is_file() {
                return Str::new(ctx, base.to_string_lossy(), true).into();
            }
            load_path = load_path.cdr();
        }
        let first = ctx.globals().loc_load_path().get();
        if first.is_pair() {
            let base = PathBuf::from(first.car().downcast::<Str>().to_string());
            return Str::new(ctx, base.to_string_lossy(), true).into();
        }
        Value::new(false)
    }
}

/// Scans the first form of `text`. When it is `(|#%require| spec ...)`, returns the specs.
fn scan_boot_requires<'gc>(ctx: MutationContext<'gc>, text: &str) -> Option<Value<'gc>> {
    let bytes: Vec<char> = text.chars().collect();
    let mut pos = 0;

    fn skip<'a>(b: &'a [char], pos: &mut usize) {
        loop {
            match b.get(*pos) {
                Some(c) if c.is_whitespace() => *pos += 1,
                Some(';') => {
                    while b.get(*pos).is_some_and(|c| *c != '\n') {
                        *pos += 1;
                    }
                }
                Some('#') if b.get(*pos + 1) == Some(&'|') => {
                    let mut depth = 0;
                    while *pos < b.len() {
                        if b[*pos] == '#' && b.get(*pos + 1) == Some(&'|') {
                            depth += 1;
                            *pos += 2;
                        } else if b[*pos] == '|' && b.get(*pos + 1) == Some(&'#') {
                            depth -= 1;
                            *pos += 2;
                            if depth == 0 {
                                break;
                            }
                        } else {
                            *pos += 1;
                        }
                    }
                }
                _ => return,
            }
        }
    }

    fn datum<'gc>(ctx: MutationContext<'gc>, b: &[char], pos: &mut usize) -> Option<Value<'gc>> {
        skip(b, pos);
        match *b.get(*pos)? {
            '(' | '[' => {
                *pos += 1;
                let mut items = Vec::new();
                loop {
                    skip(b, pos);
                    match *b.get(*pos)? {
                        ')' | ']' => {
                            *pos += 1;
                            return Some(Value::list_from_slice(ctx, items));
                        }
                        _ => items.push(datum(ctx, b, pos)?),
                    }
                }
            }
            ')' | ']' => None,
            _ => {
                let mut name = String::new();
                let mut barred = false;
                while let Some(&c) = b.get(*pos) {
                    if c == '|' {
                        barred = !barred;
                    } else if !barred && (c.is_whitespace() || "()[];".contains(c)) {
                        break;
                    } else {
                        name.push(c);
                    }
                    *pos += 1;
                }
                if name.is_empty() {
                    return None;
                }
                Some(Symbol::from_str(ctx, &name).into())
            }
        }
    }

    let form = datum(ctx, &bytes, &mut pos)?;
    if form.is_pair() && form.car() == Value::from(Symbol::from_str(ctx, "#%require")) {
        Some(form.cdr())
    } else {
        None
    }
}

#[scheme(continuation)]
pub(crate) fn continue_loading_k(
    ir: Value<'gc>,
    _cenv: Value<'gc>,
    _unused: Value<'gc>,
) -> LoadResult<'gc> {
    let rator = call.rator.downcast::<Closure>();
    let source_and_compiled_path = rator[1].get();
    let retk = rator[2].get();

    let compiled_path = source_and_compiled_path
        .list_ref(2)
        .expect("Expected compiled path")
        .downcast::<Str>()
        .to_string();
    let result = compile_expanded_to_destination(
        ctx,
        ir,
        compiled_path.as_str(),
        CompilationOptions::default(),
        true,
        DumpArtifactsOptions::default(),
        false,
    );

    match result {
        // SAFETY: `retk` is the current-continuation token captured by
        // `load-thunk-in-vicinity-k`, which resumes the caller.
        Ok(thunk) => {
            unsafe { call.continue_to(retk, &[thunk]) };
            Ok(Value::undefined())
        }
        Err(err) => Err(err),
    }
}

fn compile_backtraces_enabled<'gc>(ctx: MutationContext<'gc>) -> bool {
    let _ = ctx;
    false
}

/// Lower, compile and write `job` on a compile thread.
pub(crate) fn run_expanded_job<'gc>(
    ctx: MutationContext<'gc>,
    job: ExpandedJob<'gc>,
) -> Result<(), Failure<'gc>> {
    begin_compilation_artifact(&job.destination.path);
    let lowered = lower_expanded_scheme(
        ctx,
        job.expanded,
        job.dump_options.dump_graph,
        job.preprocessed,
    )?;
    let mut options = job.options;
    configure_backend_dump_paths(
        &mut options.backend_dumps,
        &job.destination.path,
        job.dump_options,
    );
    dump_lowered_program_artifacts(ctx, &job.destination.path, &lowered, job.dump_options);
    compile_lowered_to_destination(
        ctx,
        lowered,
        options,
        &job.destination,
        job.unsaved.as_ref(),
    )?;
    Ok(())
}

fn compile_expanded_to_destination<'gc>(
    ctx: MutationContext<'gc>,
    expanded: Value<'gc>,
    destination: &str,
    options: CompilationOptions,
    load_thunk: bool,
    dump_options: DumpArtifactsOptions,
    preprocessed: bool,
) -> LoadResult<'gc> {
    let _phase = CompilationPhase::new(ctx);
    let destination = destination_artifact_for_current_policy(destination);
    let unsaved: Unsaved = Default::default();
    let job = ExpandedJob {
        expanded,
        destination: destination.clone(),
        options,
        dump_options: merge_compile_dump_options(dump_options),
        preprocessed,
        unsaved: Some(unsaved.clone()),
    };
    let (ctx, outcome) = compile_service::run(ctx, JobInput::Expanded(job));
    compile_service::raise_failure(ctx, outcome.unwrap_or_else(|killed| killed.unwind()))?;

    if !load_thunk {
        return Ok(Str::new(ctx, destination.path.display().to_string(), true).into());
    }

    match take_unsaved(&unsaved) {
        Some(bytes) => load_unsaved(ctx, &bytes, &destination.path),
        None => load_compiled_library(ctx, &destination),
    }
}

fn compile_dump_options<'gc>(
    ctx: MutationContext<'gc>,
    dump_selections: Option<Value<'gc>>,
) -> DumpArtifactsOptions {
    let mut options = if scm_log_level(ctx) >= 5 {
        DumpArtifactsOptions::trace()
    } else {
        DumpArtifactsOptions::default()
    };
    if let Some(mut selections) = dump_selections {
        while selections.is_pair() {
            let selection = selections.car();
            if selection.is::<Symbol>() {
                let name = selection.downcast::<Symbol>().to_string();
                options.enable(&name);
            }
            selections = selections.cdr();
        }
    }
    merge_compile_dump_options(options)
}

fn configure_backend_dump_paths(
    options: &mut BackendDumpOptions,
    destination: &Path,
    dump_options: DumpArtifactsOptions,
) {
    if !dump_options.enabled {
        return;
    }
    if dump_options.dump_bytecode {
        options.bytecode = Some(resolve_artifact_dump_path(destination, ".bc.txt"));
    }
}

fn lower_expanded_scheme<'gc>(
    ctx: MutationContext<'gc>,
    expanded: Value<'gc>,
    dump_graph: bool,
    preprocessed: bool,
) -> Result<LoweredProgram<'gc>, Value<'gc>> {
    let mut converter = TermConverter::new(ctx);
    let ir = converter.convert(expanded)?;
    lower_expanded_to_cps(
        ctx,
        ir,
        cfg!(feature = "bootstrap"),
        dump_graph,
        preprocessed,
    )
}

fn load_compiled_library<'gc>(
    ctx: MutationContext<'gc>,
    compiled_artifact: &LoadArtifact,
) -> LoadResult<'gc> {
    let libs = LIBRARY_COLLECTION.fetch(ctx);
    libs.load(compiled_artifact, ctx).map_err(|err| {
        make_io_error(
            ctx,
            "load",
            Str::new(
                ctx,
                format!(
                    "Failed to load compiled artifact {}: {err}",
                    compiled_artifact.path.display()
                ),
                true,
            )
            .into(),
            &[],
        )
    })
}

fn find_path_to_public<'gc>(
    ctx: MutationContext<'gc>,
    filename: PathBuf,
    resolve_relative: bool,
    in_vicinity: Option<PathBuf>,
    arch: Option<&str>,
) -> LoadResult<'gc> {
    match find_path_to(ctx, filename, in_vicinity, resolve_relative, arch)? {
        Some((source, full_source, compiled)) => {
            let compiled = compiled.unwrap_or_else(|| fallback_file_name(ctx, &full_source));
            Ok(list!(
                ctx,
                Str::new(ctx, source.to_string_lossy(), true),
                Str::new(ctx, full_source.to_string_lossy(), true),
                Str::new(ctx, compiled.to_string_lossy(), true)
            ))
        }
        None => Ok(Value::new(false)),
    }
}
