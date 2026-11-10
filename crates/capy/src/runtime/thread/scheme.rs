use super::*;

pub struct Scheme {
    pub(crate) mutator: Mutator<crate::Rootable!(())>,
}

impl Scheme {
    pub fn enter<F, T>(&self, f: F) -> T
    where
        F: for<'gc> FnOnce(MutationContext<'gc>) -> T,
    {
        self.mutator.mutate(|ctx, _| f(ctx))
    }

    pub fn call_value<PREP, F, R>(&self, prep: PREP, finish: F) -> R
    where
        F: for<'gc> Fn(MutationContext<'gc>, Result<Value<'gc>, Value<'gc>>) -> R,
        PREP: for<'gc> FnOnce(MutationContext<'gc>, &mut Vec<Value<'gc>>) -> Value<'gc>,
    {
        self.enter(|ctx| {
            let mut args = Vec::with_capacity(4);
            ctx.stats.start_execution();
            let rator = prep(ctx, &mut args);
            let run = crate::runtime::sni::call_function(ctx, rator, args);
            ctx.stats.end_execution();

            let result = match run {
                ExecutionResult::Ok(ok) => Ok(ok),
                ExecutionResult::Err(err) => Err(err),
            };
            finish(ctx, result)
        })
    }

    /// Calls `entry_name` in module `mod_name`.
    pub fn call<ARGS, F, R>(&self, mod_name: &str, entry_name: &str, setup: ARGS, finish: F) -> R
    where
        F: for<'gc> Fn(MutationContext<'_>, Result<Value<'gc>, Value<'gc>>) -> R,
        ARGS: for<'gc> FnOnce(MutationContext<'gc>, &mut Vec<Value<'gc>>),
    {
        self.call_value(
            move |ctx, args| {
                setup(ctx, args);
                let entry = ctx
                    .public_ref(mod_name, entry_name)
                    .expect("Entrypoint not found");
                if !entry.is::<Closure>() {
                    panic!("Entrypoint is not a procedure");
                }
                entry
            },
            finish,
        )
    }

    pub fn collect_garbage(&self) {
        self.mutator.collect_garbage();
    }

    pub fn new() -> Self {
        let t = std::time::Instant::now();
        let (scm, should_init) = Self::new_runtime();
        let runtime = t.elapsed();
        let scm = if should_init { scm.boot() } else { scm };
        if std::env::var_os("CAPY_STARTUP_DEBUG").is_some() {
            eprintln!("startup: new_runtime {runtime:?}, boot {:?}", t.elapsed() - runtime);
        }
        scm
    }

    /// Runs a static bundle, the output of `capyc --static`, without booting the runtime.
    ///
    /// `bytes` is the bundle file: a FASL image, optionally after a `#!` line. Instead of
    /// `boot/main.scm` it is instantiated against the native primitives and registered
    /// as the `(boot static)` instance, which fills the core table. Its body then runs
    /// the program with `args` as `(program-arguments)`. The body ends the process by
    /// calling `exit`, so this returns only if it does not.
    pub fn new_static(bytes: &[u8], args: &[String]) -> Self {
        let scm = Self::new_static_runtime();
        scm.run_static(bytes, args);
        scm
    }

    /// The setup half of [`Scheme::new_static`]: a runtime that is not booted, linked against
    /// the native primitives only. Nothing of the bundle has run yet.
    pub fn new_static_runtime() -> Self {
        let (scm, _) = Self::new_runtime();
        scm.enter(|ctx| crate::runtime::linklet::install_capy_primitive_instance(ctx));
        scm
    }

    /// The body half: instantiates the bundle, registers it and runs it. A caller that wants
    /// the program on the thread scheduler calls this from inside `scheduler::run_main`.
    pub fn run_static(&self, bytes: &[u8], args: &[String]) {
        self.enter(|ctx| run_static_bundle(ctx, bytes, args));
    }

    /// Creates the mutator and runs the one-time process init. The flag tells whether this
    /// call did that init, which is the one that has to boot.
    fn new_runtime() -> (Self, bool) {
        let mut should_init = false;

        // if VM is not initialized yet, we need to run init code
        SCM_INITIALIZED.call_once(|| {
            should_init = true;
            let t = std::time::Instant::now();
            crate::heap::logging::init_rust_logger();
            let mmtk_builder = crate::heap::logging::mmtk_builder();
            /*match *mmtk_builder.options.plan {
                PlanSelector::GenImmix | PlanSelector::StickyImmix | PlanSelector::GenCopy => {
                    let _ = ALLOWED_GC.set(AllowedGc::Generational).unwrap();
                }

                PlanSelector::ConcurrentImmix => {
                    let _ = ALLOWED_GC.set(AllowedGc::Concurrent).unwrap();
                }

                _ => {
                    let _ = ALLOWED_GC.set(AllowedGc::Regular).unwrap();
                }
            }*/
            GarbageCollector::init(mmtk_builder);
            if std::env::var_os("CAPY_STARTUP_DEBUG").is_some() {
                eprintln!("startup: gc init {:?}", t.elapsed());
            }
        });

        let scm = Self {
            mutator: {
                let m = Mutator::new(|mc| {
                    let ctx = MutationContext::from(mc);
                    init_weak_sets(ctx);
                    init_weak_tables(ctx);
                    init_symbols(ctx);

                    let thread_object = ThreadObject::new(mc, None);
                    thread_object.bind_current_runtime_thread();
                    let state = SchemeState::new(mc, thread_object);
                    mc.init_state(state);
                });
                let t = std::time::Instant::now();
                m.mutate(|ctx, _| {
                    crate::runtime::init_once(ctx);
                });
                if std::env::var_os("CAPY_STARTUP_DEBUG").is_some() {
                    eprintln!("startup: init_once {:?}", t.elapsed());
                }
                m
            },
        };

        (scm, should_init)
    }

    pub fn new_uninit() -> Self {
        // `new_uninit` skips `init_once`, but the thread object it allocates
        // needs the layout hooks of classes owned by higher layers.
        crate::runtime::registry::register_builtin_hooks();
        let mut should_init = false;

        // if VM is not initialized yet, we need to run init code
        SCM_INITIALIZED.call_once(|| {
            should_init = true;

            crate::heap::logging::init_rust_logger();
            let mmtk_builder = crate::heap::logging::mmtk_builder();
            GarbageCollector::init(mmtk_builder);
        });

        Self {
            mutator: {
                Mutator::new(|mc| {
                    let ctx = MutationContext::from(mc);
                    init_weak_sets(ctx);
                    init_weak_tables(ctx);
                    init_symbols(ctx);

                    let thread_object = ThreadObject::new(mc, None);
                    thread_object.bind_current_runtime_thread();
                    let state = SchemeState::new(mc, thread_object);
                    mc.init_state(state);
                })
            },
        }
    }

    /*pub fn from_image(image: &[u8]) -> Self {
        let allowed_gc = match image[0] {
            0 => AllowedGc::Generational,
            1 => AllowedGc::Concurrent,
            2 => AllowedGc::Regular,
            _ => panic!("Invalid allowed GC type in image"),
        };

        let _ = ALLOWED_GC.set(allowed_gc).unwrap();
        SCM_INITIALIZED.call_once(|| {
            let mut mmtk_builder = MMTKBuilder::new();
            allowed_gc.adjust_mmtk_options(&mut mmtk_builder.options);
            GarbageCollector::init(mmtk_builder);
        });

        let scm = Self {
            mutator: {
                let m = Mutator::new(|mc| {
                    init_weak_sets(mc);
                    init_weak_tables(mc);
                    let state = SchemeState::new(mc, ThreadObject::new(mc, None));
                    mc.init_state(state);
                });
                m
            },
        };

        //let mut decoder = lz4::Decoder::new(image).expect("Failed to create LZ4 decoder");
        //let mut image = Vec::new();
        //std::io::copy(&mut decoder, &mut image).expect("Failed to decompress image");

        scm.enter(|ctx| {
            let mut reader = ImageReader::new(ctx, image);

            let img = reader.deserialize().expect("Failed to read image");

            let entrypoint = match img.boot(ctx) {
                ExecutionResult::Ok(entry) => entry,
                _ => unreachable!(),
            };

            ctx.state().accumulator.set(entrypoint);
        });

        scm
    }*/

    fn boot(self) -> Self {
        let scm = self;
        scm.enter(|ctx| {
            crate::runtime::linklet::install_capy_primitive_instance(ctx);

            // Boot procedures run a handful of times: keep the JIT from
            // tiering them up. Released before the CLI runs.
            #[cfg(all(not(windows), any(target_arch = "x86_64", target_arch = "aarch64")))]
            let _boot_hold = crate::runtime::codegen::jit::BootHold::hold();

            let t = std::time::Instant::now();
            let thunk = load_thunk_in_vicinity::<true>(ctx, "boot/main.scm", None::<&str>, false, None)
                .expect("Failed to load boot/main.scm");
            if std::env::var_os("CAPY_STARTUP_DEBUG").is_some() {
                eprintln!("startup: load boot/main {:?}", t.elapsed());
            }
            let t = std::time::Instant::now();

            match crate::runtime::sni::call_function(ctx, thunk, []) {
                ExecutionResult::Ok(_) => {}
                ExecutionResult::Err(err) => {
                    // Fatal and final: make sure the message is visible
                    // even under default-off logging before exiting.
                    crate::heap::logging::ensure_max_level(log::LevelFilter::Error);
                    log::error!(target: crate::heap::logging::BOOT_LOG_TARGET, "Failed to boot: {err}");
                    crate::heap::sync::thread::Thread::begin_process_exit();
                    std::process::exit(1);
                }
            }
            if std::env::var_os("CAPY_STARTUP_DEBUG").is_some() {
                eprintln!("startup: run boot/main {:?}", t.elapsed());
            }
        });

        scm
    }
}

impl Default for Scheme {
    fn default() -> Self {
        Self::new()
    }
}

/// Fatal error while running a static bundle: the message goes to stderr, then exit 70
fn static_fatal(message: impl std::fmt::Display) -> ! {
    crate::heap::logging::ensure_max_level(log::LevelFilter::Error);
    log::error!(target: crate::heap::logging::BOOT_LOG_TARGET, "{message}");
    crate::heap::sync::thread::Thread::begin_process_exit();
    std::process::exit(70)
}

/// Keeps a static executable away from artifacts compiled for another link.
///
/// The bundle merges the boot linklets into one body and leaves the boot-defined `(#%capy)`
/// variables (`assertion-violation`, `call/cc`, ...) unbound, while code compiled by the
/// booted runtime imports exactly those. Code compiled here imports them from `(boot raise)`
/// and friends instead, which the booted runtime does not export. So the bundle gets its own
/// compile cache, keyed by its bytes, and does not read the installed `compiled` directories.
fn isolate_static_artifacts<'gc>(ctx: MutationContext<'gc>, payload: &[u8]) {
    use std::hash::Hasher;
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    hasher.write(payload);
    let key = hasher.finish();

    let globals = ctx.globals();
    let base = globals.loc_compile_fallback_path().get();
    if base.is::<Str>() {
        let dir = std::path::Path::new(&base.downcast::<Str>().to_string())
            .join(format!("static-{key:016x}"));
        globals
            .loc_compile_fallback_path()
            .set(ctx, Str::new(ctx, dir.to_string_lossy(), true).into());
    }
    globals.loc_load_compiled_path().set(ctx, Value::null());
}

/// Instantiates the static bundle `bytes` and runs its body, see [`Scheme::new_static`].
fn run_static_bundle<'gc>(ctx: MutationContext<'gc>, bytes: &[u8], args: &[String]) {
    let payload = crate::runtime::fasl::fasl_payload(bytes);
    isolate_static_artifacts(ctx, payload);
    let reader = crate::runtime::fasl::Reader::new_with_options(
        ctx,
        std::io::Cursor::new(payload),
        crate::runtime::vm::load::policy::get_fasl_load_options(),
    );
    let thunk = reader
        .read()
        .unwrap_or_else(|err| static_fatal(format_args!("cannot read static bundle: {err}")));

    let parts = match crate::runtime::sni::call_function(ctx, thunk, []) {
        ExecutionResult::Ok(value) if value.is::<Vector>() => value,
        ExecutionResult::Ok(_) => static_fatal("not a static bundle"),
        ExecutionResult::Err(err) => static_fatal(err),
    };
    let parts = parts.downcast::<Vector>();
    let [head, name, imports, exports, body, meta] = parts.as_slice() else {
        static_fatal("not a static bundle: expected a linklet")
    };
    if *head != ctx.intern("#%linklet") {
        static_fatal("not a static bundle: expected a linklet")
    }

    let linklet = Linklet::new(ctx, *name, *imports, *exports, *body, *meta);
    let target = LibraryInstance::new(ctx, *name, false);
    let reg = root_registry(ctx);
    let capy = ctx.globals().capy_instance();
    let slots = instantiate_linklet(ctx, reg, linklet, &[capy], target)
        .unwrap_or_else(|bad| static_fatal(format_args!("static bundle: unbound import {bad}")));

    registry_register(ctx, reg, *name, target);

    let argv = args.iter().rev().fold(Value::null(), |tail, arg| {
        Value::cons(ctx, Str::new(ctx, arg, true).into(), tail)
    });
    program_arguments_fluid(ctx).set(ctx, argv);

    match crate::runtime::sni::call_function(ctx, *body, slots) {
        ExecutionResult::Ok(_) => {}
        ExecutionResult::Err(err) => static_fatal(err),
    }
}

pub(crate) static SCM_INITIALIZED: Once = Once::new();
