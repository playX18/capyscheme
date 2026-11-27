use std::path::{Path, PathBuf};

use crate::list;
use crate::prelude::*;
use crate::runtime::objectmodel::{Closure, Str, Symbol, Value};
#[cfg(feature = "bootstrap")]
use crate::runtime::vm::libraries;
use crate::runtime::vm::thunks::make_io_error;
#[cfg(feature = "bootstrap")]
use crate::runtime::vm::thunks::make_lexical_violation;
use capy_derive::scheme;

#[cfg(feature = "bootstrap")]
use super::artifact::LoadArtifact;
#[cfg(feature = "bootstrap")]
use super::compile::{
    CompilationPhase, Unsaved, destination_artifact_for_current_policy, load_unsaved, take_unsaved,
};
#[cfg(feature = "bootstrap")]
use super::compile::{ExpandedJob, compile_lowered_to_destination};
use super::hash;
use super::paths::{fallback_file_name, find_path_to};
use super::{
    compile::{load_thunk_candidates, load_thunk_in_vicinity},
    policy::set_fasl_debug_entries,
};
#[cfg(feature = "bootstrap")]
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

    /// `(%compile-seed expanded destination [load-thunk? [dump-selections [preprocessed]]])`:
    /// compiles the expanded Scheme EXPANDED (the output of psyntax) to the artifact
    /// DESTINATION with the seed emitter and returns its load thunk. Bootstrap builds only:
    /// the installed runtime has no compiler here, `(core compiler)` does this in Scheme.
    /// The trailing arguments are accepted for the callers of `%compile` and ignored.
    #[scheme(name = "%compile-seed")]
    pub fn compile_seed(
        expanded: Value<'gc>,
        destination: StringRef<'gc>,
        load_thunk: Option<bool>,
        dump_selections: Option<Value<'gc>>,
        preprocessed: Option<bool>,
    ) -> LoadResult<'gc> {
        let _ = (dump_selections, preprocessed);
        #[cfg(feature = "bootstrap")]
        {
            compile_expanded_to_destination(
                ctx,
                expanded,
                destination.to_string().as_str(),
                load_thunk.unwrap_or(true),
            )
        }
        #[cfg(not(feature = "bootstrap"))]
        {
            let _ = (expanded, destination, load_thunk);
            Err(make_io_error(
                ctx,
                "%compile-seed",
                Str::new(
                    ctx,
                    "%compile-seed is not available in non-bootstrap builds",
                    true,
                )
                .into(),
                &[],
            ))
        }
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

    /// `(%load-program-bytevector bv)`: the load thunk of the FASL code image in the
    /// bytevector BV, what `%compile` returns with `load-thunk?` true. BV is the output of
    /// `fasl-write->bytevector` on a program datum.
    #[scheme(name = "%load-program-bytevector")]
    pub fn load_program_bytevector(bv: Gc<'gc, ByteVector>) -> LoadResult<'gc> {
        super::super::compile::load_unsaved(ctx, bv.as_slice(), Path::new("<bytevector>"))
    }

    /// `(%store-compiled-image bv path)`: writes the compiled image BV (the output of
    /// `fasl-write->bytevector` on a program datum) to the artifact PATH, as `%compile` does:
    /// extension, lock and atomic rename. Returns the path written, or #f when an image for
    /// the compile cache cannot be stored (the caller then loads BV from memory).
    #[scheme(name = "%store-compiled-image")]
    pub fn store_compiled_image(bv: Gc<'gc, ByteVector>, path: StringRef<'gc>) -> LoadResult<'gc> {
        match super::super::compile::store_compiled_image(
            ctx,
            path.to_string().as_str(),
            bv.as_slice(),
        )? {
            Some(path) => Ok(Str::new(ctx, path.display().to_string(), true).into()),
            None => Ok(Value::new(false)),
        }
    }

    /// `(%bytecode-isa)`: the instruction set of `bytecode/def.rs` as an alist, see
    /// `boundary::bytecode_isa`. `tools/gen-isa.scm` writes it to
    /// `core/compiler/bytecode/isa.scm`.
    #[scheme(name = "%bytecode-isa")]
    pub fn bytecode_isa() -> LoadResult<'gc> {
        super::super::boundary::bytecode_isa(ctx).map_err(|message| {
            make_io_error(
                ctx,
                "%bytecode-isa",
                Str::new(ctx, message, true).into(),
                &[],
            )
        })
    }

    /// `(%primitive-table)`: what the compiler asks of each primitive, see
    /// `boundary::primitive_table`.
    #[scheme(name = "%primitive-table")]
    pub fn primitive_table() -> Value<'gc> {
        super::super::boundary::primitive_table(ctx)
    }

    /// `(try-load-thunk-in-vicinity filename resolve-relative [vicinity])`: the thunk of a
    /// compiled file named by FILENAME, or for a source file the list
    /// `(source full-source cache-artifact candidates)`: the artifact the source is compiled
    /// to in the compile cache, and the existing artifacts that may hold its compiled form.
    /// Which of them is fresh is for the caller to decide, see `fasl-needs-recompile?`.
    #[scheme(name = "try-load-thunk-in-vicinity")]
    pub fn scm_try_load_thunk_in_vicinity(
        filename: StringRef<'gc>,
        resolve_relative: bool,
        in_vicinity: OptionalStringRef<'gc>,
    ) -> LoadResult<'gc> {
        let filename = PathBuf::from(filename.as_gc_ref().to_string());
        let in_vicinity = in_vicinity.map(|s| PathBuf::from(s.as_gc_ref().to_string()));
        let result = load_thunk_candidates(ctx, filename, in_vicinity, resolve_relative);
        result
    }

    /// `(%file-hash path)`: the hash of the bytes of the file, or `#f` when it cannot be read.
    #[scheme(name = "%file-hash")]
    pub fn file_hash(path: StringRef<'gc>) -> Value<'gc> {
        match std::fs::read(path.to_string()) {
            Ok(bytes) => Str::new(ctx, super::hash::hash_bytes(&bytes), true).into(),
            Err(_) => Value::new(false),
        }
    }

    /// `(%datum-hash datum)`: the hash of a datum, see `fasl` decls.
    #[scheme(name = "%datum-hash")]
    pub fn datum_hash(datum: Value<'gc>) -> Value<'gc> {
        Str::new(ctx, super::hash::hash_datum(datum), true).into()
    }

    /// `(%compiler-id)`: the id of this compiler, see `compiler_id`.
    #[scheme(name = "%compiler-id")]
    pub fn compiler_id() -> Value<'gc> {
        Str::new(ctx, super::hash::compiler_id(), true).into()
    }

    /// `(%fasl-directory path)`: the directory header of the FASL image `path` as
    /// `fasl-read-directory` reads it, or `#f` when the file is missing, is not an image
    /// of this compiler, or has no directory.
    #[scheme(name = "%fasl-directory")]
    pub fn fasl_directory(path: StringRef<'gc>) -> Value<'gc> {
        use crate::runtime::fasl::Reader;
        let result = std::fs::File::open(path.to_string())
            .and_then(|file| Reader::new(ctx, file).read_directory());
        match result {
            Ok(Some(directory)) => directory,
            _ => Value::new(false),
        }
    }

    /// `(%file-newer? a b)`: whether the file `a` was modified after the file `b`. A missing
    /// file counts as older than anything.
    #[scheme(name = "%file-newer?")]
    pub fn file_newer(a: StringRef<'gc>, b: StringRef<'gc>) -> bool {
        let time = |path: StringRef<'gc>| {
            std::fs::metadata(path.to_string())
                .and_then(|metadata| metadata.modified())
                .ok()
        };
        match (time(a), time(b)) {
            (Some(a), Some(b)) => a > b,
            (Some(_), None) => true,
            _ => false,
        }
    }

    /// `(%static-bundle-file? path)`: whether the file is a static bundle, the output of
    /// `capyc --static`. A bundle carries its own boot linklets, so it cannot be loaded into
    /// a booted image, see `%run-static-bundle`.
    #[scheme(name = "%static-bundle-file?")]
    pub fn static_bundle_file(path: StringRef<'gc>) -> bool {
        use std::io::Read;
        let mut head = Vec::new();
        std::fs::File::open(path.to_string())
            .and_then(|file| file.take(4096).read_to_end(&mut head))
            .is_ok()
            && crate::runtime::fasl::is_static_bundle_image(&head)
    }

    /// `(%run-static-bundle path args)`: runs the static bundle in a process of its own, as
    /// `capy path args...` does, and returns its exit status. The standard streams are shared.
    #[scheme(name = "%run-static-bundle")]
    pub fn run_static_bundle_process(path: StringRef<'gc>, args: Value<'gc>) -> LoadResult<'gc> {
        let mut command = std::process::Command::new(std::env::current_exe().map_err(|err| {
            make_io_error(
                ctx,
                "%run-static-bundle",
                Str::new(
                    ctx,
                    format!("cannot find the running executable: {err}"),
                    true,
                )
                .into(),
                &[],
            )
        })?);
        command.arg(path.to_string());
        let mut rest = args;
        while rest.is_pair() {
            command.arg(rest.car().downcast::<Str>().to_string());
            rest = rest.cdr();
        }
        match command.status() {
            Ok(status) => Ok(Value::new(status.code().unwrap_or(1))),
            Err(err) => Err(make_io_error(
                ctx,
                "%run-static-bundle",
                Str::new(ctx, format!("cannot run {}: {err}", path.to_string()), true).into(),
                &[],
            )),
        }
    }

    /// `(%cache-artifact-path source)`: where the compile cache keeps the compiled form of
    /// the source file `source`, the same file the loader looks in.
    #[scheme(name = "%cache-artifact-path")]
    pub fn cache_artifact_path(source: StringRef<'gc>) -> Value<'gc> {
        let path = fallback_file_name(ctx, PathBuf::from(source.to_string()));
        Str::new(ctx, path.to_string_lossy(), true).into()
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
                    // `k` also gets the path of the file, to hash its source: the decl of a
                    // library in it is the same as when the file is compiled the usual way.
                    let source = Value::from(Str::new(ctx, &file, true));
                    call.call_with(k, &[program, env, source], after_call.into());
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
pub(crate) fn continue_loading_k(ir: Value<'gc>) -> LoadResult<'gc> {
    let rator = call.rator.downcast::<Closure>();
    let source_and_compiled_path = rator[1].get();
    let retk = rator[2].get();

    let compiled_path = source_and_compiled_path
        .list_ref(2)
        .expect("Expected compiled path")
        .downcast::<Str>()
        .to_string();
    #[cfg(feature = "bootstrap")]
    let result = compile_expanded_to_destination(ctx, ir, compiled_path.as_str(), true);
    #[cfg(not(feature = "bootstrap"))]
    let result: LoadResult<'gc> = {
        let _ = (ir, compiled_path);
        Err(make_io_error(
            ctx,
            "load",
            Str::new(ctx, "the runtime has no built-in compiler", true).into(),
            &[],
        ))
    };

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

/// Seed-compile and write `job` on a compile thread.
#[cfg(feature = "bootstrap")]
pub(crate) fn run_expanded_job<'gc>(
    ctx: MutationContext<'gc>,
    job: ExpandedJob<'gc>,
) -> Result<(), Failure<'gc>> {
    let mut converter = crate::runtime::builtins::expand::TermConverter::new(ctx);
    let ir = converter.convert(job.expanded)?;
    let lowered = crate::compiler::direct::lower_term(ctx, ir)?;
    compile_lowered_to_destination(ctx, lowered, &job.destination, job.unsaved.as_ref())?;
    Ok(())
}

#[cfg(feature = "bootstrap")]
fn compile_expanded_to_destination<'gc>(
    ctx: MutationContext<'gc>,
    expanded: Value<'gc>,
    destination: &str,
    load_thunk: bool,
) -> LoadResult<'gc> {
    let _phase = CompilationPhase::new(ctx);
    let destination = destination_artifact_for_current_policy(destination);
    let unsaved: Unsaved = Default::default();
    let job = ExpandedJob {
        expanded,
        destination: destination.clone(),
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

#[cfg(feature = "bootstrap")]
fn load_compiled_library<'gc>(
    ctx: MutationContext<'gc>,
    compiled_artifact: &LoadArtifact,
) -> LoadResult<'gc> {
    libraries::load_artifact(ctx, compiled_artifact).map_err(|err| {
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
