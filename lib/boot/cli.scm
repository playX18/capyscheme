(library (boot cli)
  (export enter eval-string eval-string/value enter-compiler)
  (import (capy)
    (core control)
    (core repl)
    (capy args))

  (define (cli-error fmt . args)
    (apply format (current-error-port) fmt args)
    (exit 1))

  (define (eval-string/value str)
    (call-with-input-string
      str
      (lambda (in)
        (define reader (make-reader in "stdin"))
        (let lp ([result (if #f #f)])
          (let ([exp (read-datum reader)])
            (if (not (eof-object? exp))
              (lp (eval exp (current-module)))
              result))))))

  (define (eval-string str)
    (eval-string/value str))


  (define (install-core-in-user-module!)
    (module-use! (resolve-module '(capy user) #f #t)
      (resolve-interface '(core) #f '() #f)))

  (define (canonicalize-paths paths)
    (map canonicalize-path-string paths))

  ;; Generated from FLAG_DEFS in crates/capy/src/utils/flags.rs. The launcher
  ;; turns these into environment variables before Scheme starts, so the
  ;; entries here only make the options appear in --help.
  (define (add-flag-options! parser)
    (argparser-add-separator! parser "Compiler tuning options:")
    (add-option! parser
      "share-min-overlap"
      (defaults-to "2")
      (value-help "N")
      (help "Min record-resident free vars per member for a site to join a shared env record."))
    (add-option! parser
      "share-chain-cost"
      (defaults-to "3")
      (value-help "N")
      (help "Cross-site chain-head record cost in words."))
    (add-flag! parser
      "share-order"
      (defaults-to #f)
      not-negatable
      (help "Only share a closure environment when every use of the shared variables comes before every use of the member's own captures."))
    (add-flag! parser
      "share-dump"
      (defaults-to #f)
      not-negatable
      (help "Print per-site closure-sharing decisions (allocate/reuse, record size, members) and flow stats. Toggles log target `capy::share`."))
    (add-flag! parser
      "closure-sharing"
      (defaults-to #t)
      (help "Let closures share environment records instead of each allocating its own."))
    (add-option! parser
      "gcps-contify"
      (defaults-to "dom")
      (allowed (list "0" "off" "none" "false" "scc" "legacy" "both" "dom+scc" "dom-then-scc" "dom" "dominator" "dominators"))
      (value-help "MODE")
      (help "Contification strategy: off (or 0), scc, dom, or both (dom+scc)."))
    (add-flag! parser
      "profile-passes"
      (defaults-to #f)
      not-negatable
      (help "Emit `;; PERF (capy)` phase timing lines to stderr."))
    (add-flag! parser
      "strip-spans"
      (defaults-to #f)
      not-negatable
      (help "Omit bytecode source maps from emitted FASLs (backtraces fall back to procedure metadata)."))
    (argparser-add-separator! parser "Dump options:")
    (add-option! parser
      "dump-compile"
      (allowed (list "ir" "graph" "gcps" "ssa" "bytecode" "bc" "asm" "disassembly" "noopt" "all" "1"))
      (value-help "LIST")
      (help "Per-compilation artifact dumps (comma-separated): ir, gcps, ssa, bytecode, noopt, or all."))
    (add-flag! parser
      "compile-dump-noopt"
      (defaults-to #f)
      not-negatable
      (help "Also dump the unoptimized (pre-lowering) IR."))
    (add-option! parser
      "compile-dump-dir"
      (value-help "PATH")
      (help "Directory for CAPY_COMPILE_DUMP artifacts (fallback: CAPY_DUMP_DIR or ./capy-dumps)."))
    (add-option! parser
      "compile-dump-limit"
      (defaults-to "max")
      (value-help "N")
      (help "Cap the number of compilations that dump artifacts (max = unlimited)."))
    (add-option! parser
      "dump-gcps-contify"
      (allowed (list "0" "off" "none" "false" "1" "on" "true" "all" "scc" "legacy" "dom" "dominator" "dominators"))
      (value-help "LIST")
      (help "GCPS contification dumps: off/0, scc, dom, or all/1 for both sources."))
    (add-option! parser
      "gcps-dump-dir"
      (value-help "PATH")
      (help "Directory for CAPY_GCPS_DUMP_CONTIFY dumps (fallback: CAPY_DUMP_DIR or ./capy-dumps)."))
    (add-option! parser
      "gcps-dump-limit"
      (defaults-to "max")
      (value-help "N")
      (help "Cap the number of GCPS contification dumps (max = unlimited)."))
    (add-option! parser
      "dump-dir"
      (value-help "PATH")
      (help "Shared fallback directory for all dump kinds (default: ./capy-dumps)."))
    (argparser-add-separator! parser "Garbage collection options:")
    (add-option! parser
      "gc-heuristic"
      (defaults-to "adaptive")
      (allowed (list "adaptive" "static" "compact" "aggressive" "passive" "generational"))
      (value-help "MODE")
      (help "GC trigger heuristic: adaptive, static, compact, aggressive, passive, or generational."))
    (add-option! parser
      "gc-max-heap"
      (defaults-to "2G")
      (value-help "SIZE")
      (help "Maximum heap size with optional K/M/G suffix (e.g. 2G)."))
    (add-option! parser
      "gc-min-free-percent"
      (defaults-to "10")
      (value-help "N")
      (help "Minimum free heap threshold, percent of the heap (clamped to 0-100)."))
    (add-option! parser
      "gc-init-free-percent"
      (defaults-to "70")
      (value-help "N")
      (help "Initial free heap threshold, percent of the heap (clamped to 0-100)."))
    (add-option! parser
      "gc-allocation-threshold-percent"
      (defaults-to "0")
      (value-help "N")
      (help "Allocation-since-GC threshold, percent of the heap (compact mode; clamped to 0-100)."))
    (add-option! parser
      "gc-alloc-spike-percent"
      (defaults-to "5")
      (value-help "N")
      (help "Allocation spike detection factor, percent (clamped to 0-100)."))
    (add-option! parser
      "gc-learning-steps"
      (defaults-to "5")
      (value-help "N")
      (help "History length used by the adaptive heuristic."))
    (add-option! parser
      "gc-adaptive-confidence"
      (defaults-to "1.8")
      (value-help "N")
      (help "Confidence multiplier for the adaptive heuristic."))
    (add-option! parser
      "gc-adaptive-spike-threshold"
      (defaults-to "1.8")
      (value-help "N")
      (help "Spike threshold for the adaptive heuristic."))
    (add-option! parser
      "gc-acceleration-sample-period-ms"
      (defaults-to "15")
      (value-help "N")
      (help "Acceleration sampling period in milliseconds."))
    (add-option! parser
      "gc-acceleration-sample-count"
      (defaults-to "8")
      (value-help "N")
      (help "Number of acceleration samples."))
    (add-option! parser
      "gc-momentary-spike-sample-count"
      (defaults-to "3")
      (value-help "N")
      (help "Number of momentary spike samples."))
    (add-option! parser
      "gc-guaranteed-interval-ms"
      (defaults-to "300000")
      (value-help "N")
      (help "Force a GC at least this often; 0 = off."))
    (add-option! parser
      "gc-plan"
      (defaults-to "GenImmix")
      (allowed (list "StickyImmix" "ConcurrentImmix" "MarkSweep" "Immix" "SemiSpace" "GenImmix" "GenCopy"))
      (value-help "PLAN")
      (help "MMTk GC plan."))
    (add-option! parser
      "gc-trigger"
      (value-help "TEXT")
      (help "MMTk GC trigger override; also read by MMTk itself, capy uses it for the default max heap."))
    (argparser-add-separator! parser "Engine options:")
    (add-option! parser
      "engine"
      (allowed (list "rust" "template"))
      (value-help "MODE")
      (help "Execution engine: rust, template, or unset for the build default."))
    (add-flag! parser
      "template-alloc-stats"
      (defaults-to #f)
      not-negatable
      (help "Count generated-code allocations (fast path vs capy_op_alloc) and report at exit."))
    (add-flag! parser
      "template-bail-stats"
      (defaults-to #f)
      not-negatable
      (help "Report bailed-opcode counts of the template interpreter at exit."))
    (add-flag! parser
      "template-bail-oneway"
      (defaults-to #f)
      not-negatable
      (help "Make capy_bail one-way: stay in the Rust engine after the first bail."))
    (add-flag! parser
      "template-op-stats"
      (defaults-to #f)
      not-negatable
      (help "Count executed opcodes in the template interpreter (emitted at generation time) and report at exit."))
    (add-option! parser
      "template-bail"
      (value-help "TEXT")
      (help "Comma-separated opcode names forced to bail to the Rust engine."))
    (add-flag! parser
      "prim-stats"
      (defaults-to #f)
      not-negatable
      (help "Count primitive calls and report at exit."))
    (add-flag! parser
      "fasl-debug"
      (defaults-to #f)
      not-negatable
      (help "Load FASL files with debug entries."))
    (add-option! parser
      "profile-out"
      (value-help "PATH")
      (help "Arm the SIGPROF profiler at startup and write folded stacks to this file."))
    (add-flag! parser
      "ic-log"
      (defaults-to #f)
      not-negatable
      (help "Log generic-dispatch slot-protocol fires (inline-cache invalidations). Toggles log target `capy::ic`."))
    (add-option! parser
      "rust-log"
      (value-help "TEXT")
      (help "Log filter in env_logger syntax (e.g. info, capy=debug); mmtk stays silent unless named."))
    (add-option! parser
      "compile-threads"
      (defaults-to "0")
      (value-help "N")
      (help "Background compile service threads; 0 means one per core, at most 2."))
    (add-option! parser
      "compile-stack-kb"
      (defaults-to "0")
      (value-help "N")
      (help "Stack size of compile service threads in KiB; 0 means 262144 (256 MiB)."))
    (argparser-add-separator! parser "JIT options:")
    (add-flag! parser
      "jit"
      (defaults-to #t)
      (help "Enable Just In Time compilation."))
    (add-option! parser
      "jit-threshold"
      (defaults-to "10")
      (value-help "N")
      (help "Entries or back-edges before a procedure compiles."))
    (add-option! parser
      "jit-max-code"
      (defaults-to "max")
      (value-help "N")
      (help "Total live compiled code in bytes past which tier-up stops (max = unlimited)."))
    (add-option! parser
      "jit-max-code-per-block"
      (defaults-to "max")
      (value-help "N")
      (help "Largest single compiled code block in bytes; larger procedures stay interpreted (max = unlimited)."))
    (add-option! parser
      "jit-limit"
      (defaults-to "max")
      (value-help "N")
      (help "Compile only the first n procedures, to bisect a miscompile (max = unlimited)."))
    (add-flag! parser
      "jit-log"
      (defaults-to #f)
      not-negatable
      (help "Report compiles and returns to compiled code."))
    (add-flag! parser
      "jit-log-verbose"
      (defaults-to #f)
      not-negatable
      (help "Add bulky per-procedure detail (bytecode dump, address maps, control-flow trace) to the JIT log."))
    (add-flag! parser
      "jit-log-resume"
      (defaults-to #f)
      not-negatable
      (help "Report every resume into compiled code (very verbose)."))
    (add-flag! parser
      "jit-stats"
      (defaults-to #f)
      not-negatable
      (help "Report compile, resume and slow-path counts at exit."))
    (add-option! parser
      "jit-dump-dir"
      (value-help "PATH")
      (help "Write each compiled procedure's listing and ELF to this directory."))
    (add-option! parser
      "jit-workers"
      (defaults-to "1")
      (value-help "N")
      (help "Tier-up worker threads; 0 compiles synchronously on the requesting thread."))
    (add-option! parser
      "jit-tiers"
      (value-help "TEXT")
      (help "Comma-separated tiers allowed to compile (default: baseline)."))
    (add-flag! parser
      "jit-gdb"
      (defaults-to #f)
      not-negatable
      (help "Register every loaded image with GDB's JIT interface (copies all image code)."))
    (add-option! parser
      "jit-filter"
      (value-help "TEXT")
      (help "Compile only procedures whose metadata contains this text, to bisect a miscompile."))
    (add-flag! parser
      "perf-map"
      (defaults-to #f)
      not-negatable
      (help "Append loaded image symbols to /tmp/perf-<pid>.map for perf."))
    (argparser-add-separator! parser "Scheduler options:")
    (add-option! parser
      "workers"
      (defaults-to "0")
      (value-help "N")
      (help "Thread scheduler worker threads; 0 means one per core."))
    (add-option! parser
      "thread-stack-kb"
      (defaults-to "0")
      (value-help "N")
      (help "Thread stack size in KiB; 0 means 2048 (2 MiB)."))
    (add-option! parser
      "thread-quantum-ms"
      (defaults-to "0")
      (value-help "N")
      (help "Thread preemption quantum in milliseconds; 0 means 10."))
    (add-flag! parser
      "signal-preempt"
      (defaults-to #t)
      (help "Send SIGURG to preempt long-running threads. Set to 0 to rely on the cooperative flag alone."))
    (if #f #f))

  (define (parse-entrypoint str)
    (read (open-input-string str)))

  (define (enter args)
    "Main entrypoint of CapyScheme CLI."
    (define arg0 "capy")
    (define interactive? #t)
    (define entrypoint #f)
    (define load-path '())
    (define load-compiled-path '())
    (define extensions '())

    (define parser (argparser))

    (define (finish args out)
      (set-program-arguments! (cons arg0 args))
      (if (not (zero? (log:max-level)))
        (log:set-logger! *simple-logger*))
      (with-exception-handler
        (lambda (c)
          (flush-output-port (current-output-port))
          ((current-exception-printer) c)
          (exit 1))
        (lambda ()
          (eval `(begin
                  ,@(reverse out)
                  ,(if interactive?
                    `((|@| (core repl) read-eval-print-loop))
                    '(exit 0)))))))
    (define (run)
      (with-exception-handler
        (lambda (c)
          (format (current-error-port) "Failed to parse command line arguments: ~a: ~a~%"
            (if (who-condition? c) (condition-who c) '?)
            (if (message-condition? c) (condition-message c) c))
          (exit 1))
        (lambda ()
          (define res (parse-args parser (cdr args)))

          (when (arg-results-ref res "version")
            (format #t "CapyScheme ~a~%" (implementation-version))
            (exit 0))

          (if (arg-results-ref res "log-warn")
            (log:set-max-level! log:warn))
          (if (arg-results-ref res "log-error")
            (log:set-max-level! log:error))
          (if (arg-results-ref res "log-info")
            (log:set-max-level! log:info))
          (if (arg-results-ref res "log-debug")
            (log:set-max-level! log:debug))
          (if (arg-results-ref res "log-trace")
            (begin
              (log:set-max-level! log:trace)
              ((|@@| (capy) gc-logging-enable!) #t)))
          (if (not (zero? (log:max-level)))
            (log:set-logger! *simple-logger*))
          (when (arg-results-ref res "help")
            (format #t "CapyScheme ~a~%" (implementation-version))
            (format #t "Usage:~%~a~%" (argparser-usage parser))
            (exit 0))
          (when (arg-results-ref res "entrypoint")
            (set! entrypoint (parse-entrypoint (arg-results-ref res "entrypoint"))))
          (when (and (arg-results-ref res "r7rs") (arg-results-ref res "r6rs"))
            (cli-error "Cannot specify both --r7rs and --r6rs modes"))
          (cond
            [(arg-results-ref res "r7rs") (install-r7rs!)]
            [(arg-results-ref res "r6rs") (install-r6rs!)])
          (set! %load-path (append (canonicalize-paths (reverse (arg-results-ref res "load-path"))) %load-path))
          (set! %load-path (append %load-path (canonicalize-paths (reverse (arg-results-ref res "append-load-path")))))
          (set! %load-compiled-path (append (canonicalize-paths (reverse (arg-results-ref res "compiled-load-path"))) %load-compiled-path))
          (set! %load-extensions (append (reverse (arg-results-ref res "extensions")) %load-extensions))
          (install-core-in-user-module!)
          (if (arg-results-ref res "fresh-auto-compile")
            (set! %fresh-auto-compile #t))
          (if (arg-results-ref res "debug")
            ((|@@| (capy) load-debug-enable!) #t))
          (define out '())

          (when (arg-results-ref res "runtime-stats")
            (set! out (cons `((|@@| (capy) runtime-stats-enable!) #t) out)))

          (when (arg-results-ref res "load")
            (set! out (cons `((|@@| (capy) load) ,(arg-results-ref res "load")) out)))
          (when (arg-results-ref res "script")
            (set! arg0 (arg-results-ref res "script"))
            (set! interactive? #f)
            (set! out (cons `((|@@| (capy) load) ,(arg-results-ref res "script")) out)))
          (when (arg-results-ref res "command")
            (set! interactive? #f)
            (set! out (cons `((|@@| (boot cli) eval-string) ,(arg-results-ref res "command")) out)))
          (when (pair? (arg-results-ref res "doc-module"))
            (set! interactive? #f)
            (set! out (cons `((|@@| (capy docgen) docgen-main)
                              (list ,@(arg-results-ref res "doc-module"))
                              ,(arg-results-ref res "doc-output"))
                            out)))
          ;; `capy file.scm args...`: run the first positional as a script, the
          ;; rest stay available as program arguments.
          (let ([files (arg-results-rest res)])
            (when (and interactive? (pair? files))
              (set! interactive? #f)
              (set! out (cons `((|@@| (capy) load) ,(car files)) out))))
          (when entrypoint
            (set! interactive? #f)
            (set! out (cons `(,entrypoint) out)))
          (when (and (arg-results-ref res "runtime-stats") (not interactive?))
            (set! out (cons `((|@@| (capy) format) #t "~a~%" ((|@@| (capy) runtime-stats-report))) out)))
          (finish (arg-results-rest res) out))))

    (add-flag! parser
      "r7rs"
      not-negatable
      (help "Run in R7RS compatibility mode"))
    (add-flag! parser
      "r6rs"
      not-negatable
      (help "Run in R6RS compatibility mode"))
    (add-flag! parser
      "help"
      not-negatable
      (help "Show this help message and exit"))
    (add-flag! parser
      "help-flags"
      (aliases '("dump-flags"))
      not-negatable
      (help "List every flag (env var and CLI alias) with current value and exit"))
    (add-option! parser
      "script"
      (abbreviation "s")
      (help "Run specified file as a script")
      (value-help "FILE"))
    (add-option! parser
      "command"
      (abbreviation "c")
      (help "Evaluate a command string")
      (value-help "STRING"))
    (add-multi-option! parser
      "doc-module"
      (defaults-to '())
      (value-help "MODULE")
      split-commas
      (help "Document a library module as JSON and exit (e.g. --doc-module '(core lists)'); repeatable"))
    (add-option! parser
      "doc-output"
      (help "Write --doc-module JSON output to FILE instead of stdout")
      (value-help "FILE"))
    (add-option! parser
      "load"
      (abbreviation "l")
      (help "Load specified file before entering REPL or running script")
      (value-help "FILE"))
    (add-multi-option! parser
      "load-path"
      (abbreviation "L")
      (defaults-to '())
      (value-help "DIR")
      split-commas
      (help "Add a directory to the load path"))
    (add-multi-option! parser
      "append-load-path"
      (abbreviation "A")
      (defaults-to '())
      (value-help "DIR")
      split-commas
      (help "Append a directory to the load path"))
    (add-multi-option! parser
      "compiled-load-path"
      (abbreviation "C")
      (defaults-to '())
      (value-help "DIR")
      split-commas
      (help "Add a directory to the compiled load path"))
    (add-multi-option! parser
      "extensions"
      (abbreviation "x")
      (defaults-to '())
      (value-help "EXTENSION")
      split-commas
      (help "Add specified extension to list of extensions"))
    (add-option! parser
      "entrypoint"
      (abbreviation "e")
      (defaults-to #f)
      (value-help "EXPR")
      (help "Specify the entrypoint function to call"))
    (add-flag! parser
      "fresh-auto-compile"
      (defaults-to #f)
      not-negatable
      (help "Enable fresh auto compilation of loaded files"))
    (add-flag! parser
      "runtime-stats"
      (defaults-to #f)
      not-negatable
      (help "Enable runtime timing/counter statistics"))
    (add-flag! parser
      "debug"
      (defaults-to #f)
      not-negatable
      (help "Enable debug stack traces for FASL code (including stdlib)"))
    (add-flag-options! parser)
    (argparser-add-separator! parser "Logging options:")
    (add-flag! parser
      "log-trace"
      not-negatable
      (help "Enable trace logging"))
    (add-flag! parser
      "log-info"
      not-negatable
      (help "Enable info logging"))
    (add-flag! parser
      "log-debug"
      not-negatable
      (help "Enable debug logging"))
    (add-flag! parser
      "log-warn"
      not-negatable
      (help "Enable warn logging"))
    (add-flag! parser
      "log-error"
      not-negatable
      (help "Enable error logging"))
    (add-flag! parser
      "version"
      not-negatable
      (help "Show version and exit"))

    (run)))

(define (enter-compiler args)
  "Entrypoint for `capyc` compiler CLI."
  ((|@@| (capy) primitive-load) "boot/compiler.scm")

  (define arg0 "capyc")
  (define parser (argparser))

  (define (print-help)
    (format #t "CapyScheme Compiler ~a~%" (implementation-version))
    (format #t "Usage:~%~a~%" (argparser-usage parser))
    (exit 0))

  (define (parse-module str)
    (and str
      (let* ([len (string-length str)]
             [body
               (if (and (>= len 2)
                    (eqv? (string-ref str 0) #\()
                    (eqv? (string-ref str (- len 1)) #\)))
                 (substring str 1 (- len 1))
                 str)]
             [parts (filter (lambda (part) (> (string-length part) 0))
                     (string-split body #\space))])
        (map string->symbol parts))))

  (define (run)
    (define res (with-exception-handler
                 (lambda (c)
                   (format (current-error-port) "Failed to parse command line arguments: ~a~%" (condition-message c))
                   (exit 1))
                 (lambda ()
                   (parse-args parser (cdr args)))))

    (define load-path (arg-results-ref res "load-path"))
    (define append-load-path (arg-results-ref res "append-load-path"))
    (define compiled-load-path (arg-results-ref res "compiled-load-path"))
    (define extensions (arg-results-ref res "extensions"))
    (define out-file (arg-results-ref res "output"))
    (define r7rs-mode (arg-results-ref res "r7rs"))
    (define r6rs-mode (arg-results-ref res "r6rs"))
    (define verbose (arg-results-ref res "verbose"))
    (define runtime-stats (arg-results-ref res "runtime-stats"))
    (define version (arg-results-ref res "version"))
    (define barrier-kind (arg-results-ref res "barrier"))
    (define backtrace #t)
    (define dump-options
      (let ([options '()])
        (when (arg-results-ref res "dump-graph")
          (set! options (cons 'graph options)))
        (when (arg-results-ref res "dump-ssa")
          (set! options (cons 'ssa options)))
        (when (arg-results-ref res "dump-cranelift")
          (set! options (cons 'cranelift options)))
        (when (arg-results-ref res "dump-disassembly")
          (set! options (cons 'disassembly options)))
        (reverse options)))

    (when version (format #t "CapyScheme Compiler ~a~%" (implementation-version)) (exit 0))
    (when barrier-kind
      (unless (member barrier-kind '("nobarrier" "objbarrier" "satbbarrier"))
        (format (current-error-port)
          "error: invalid --barrier ~a; expected nobarrier, objbarrier, or satbbarrier~%"
          barrier-kind)
        (exit 1))
      (compile-barrier-kind! barrier-kind))
    (if (and r7rs-mode r6rs-mode)
      (cli-error "Cannot specify both --r7rs and --r6rs modes"))
    (cond
      [r7rs-mode (install-r7rs!)]
      [r6rs-mode (install-r6rs!)])
    (set! %load-path (append (canonicalize-paths load-path) %load-path))
    (set! %load-path (append %load-path (canonicalize-paths append-load-path)))
    (set! %load-compiled-path (append (canonicalize-paths (reverse compiled-load-path)) %load-compiled-path))
    (set! %load-extensions (append (reverse extensions) %load-extensions))
    (install-core-in-user-module!)
    (define source-files (arg-results-rest res))
    (if (null? source-files)
      (print-help))

    (when (arg-results-ref res "help")
      (print-help))

    (when runtime-stats
      ((|@@| (capy) runtime-stats-enable!) #t))
    (when (arg-results-ref res "nobacktrace")
      (format (current-error-port)
        "warning: --nobacktrace is deprecated; debug is controlled when FASL is loaded~%"))

    (define module-name (or (parse-module (arg-results-ref res "module"))
                         '(capy user)))

    (when (and out-file
           (or (null? source-files)
             (not (null? (cdr source-files)))))
      (cli-error "--output can only be used when compiling a single file"))

    (with-exception-handler
      (lambda (exn)
        (print-condition exn (current-error-port))
        (format (current-error-port) "Compilation failed.~%")
        (exit 1))
      (lambda ()
        (for-each
          (lambda (file)
            (with-exception-handler
              (lambda (exn)
                (print-condition exn (current-error-port))
                (format (current-error-port) ";; Compilation of file ~a failed.~%" file)
                (when (marks-condition? exn)
                  (stack-trace (condition-marks exn) (current-error-port)))
                (exit 1))
              (lambda ()
                (when verbose
                  (format #t ";; Compiling file ~a~%" file))
                (with-continuation-mark *compile-backtrace-key* backtrace
                  (define out (compile-file
                               file
                               out-file
                               (or (and module-name (resolve-module module-name #f #t)) #f)
                               #f
                               dump-options))
                  (when verbose
                    (format #t ";; Compiled ~a -> ~a~%" file out))))))
          source-files)
        (when runtime-stats
          ((|@@| (capy) format) #t "~a~%" ((|@@| (capy) runtime-stats-report)))))))

  (add-option! parser
    "output"
    (abbreviation "o")
    (help "Specify output file"))

  (add-flag! parser
    "nobacktrace"
    not-negatable
    (help "Deprecated compatibility flag; debug is controlled when FASL is loaded"))

  (add-flag! parser
    "r7rs"
    not-negatable
    (help "Compile in R7RS mode"))
  (add-flag! parser
    "r6rs"
    not-negatable
    (help "Compile in R6RS mode"))

  (add-flag! parser
    "verbose"
    (abbreviation "v")
    not-negatable
    (help "Enable verbose output"))

  (add-option! parser
    "barrier"
    (defaults-to #f)
    (value-help "KIND")
    (help "Force FASL write-barrier kind: nobarrier, objbarrier, or satbbarrier (default: from live --gc-plan)"))

  (add-flag! parser
    "version"
    not-negatable
    (help "Show version and exit"))

  (add-flag! parser
    "runtime-stats"
    (defaults-to #f)
    not-negatable
    (help "Enable runtime timing/counter statistics"))

  (argparser-add-separator! parser "Compiler dump options:")
  (add-flag! parser
    "dump-graph"
    (defaults-to #f)
    not-negatable
    (help "Dump graph CPS for each compiled file"))
  (add-flag! parser
    "dump-ssa"
    (defaults-to #f)
    not-negatable
    (help "Dump SSA for each compiled file"))
  (add-flag! parser
    "dump-cranelift"
    (defaults-to #f)
    not-negatable
    (help "Dump Cranelift IR for each compiled file"))
  (add-flag! parser
    "dump-disassembly"
    (defaults-to #f)
    not-negatable
    (help "Dump native-code disassembly for each compiled file"))

  (add-flag-options! parser)

  (add-multi-option! parser
    "load-path"
    (abbreviation "L")
    (help "Add a directory to the load path")
    (value-help "DIR")
    split-commas)
  (add-multi-option! parser
    "compiled-load-path"
    (abbreviation "C")
    (help "Add a directory to the compiled load path")
    (value-help "DIR")
    split-commas)

  (add-multi-option! parser
    "extensions"
    (abbreviation "x")
    (help "Add specified extension to list of extensions")
    (value-help "EXTENSION")
    split-commas)
  (add-multi-option! parser
    "append-load-path"
    (abbreviation "A")
    (help "Append a directory to the load path")
    (value-help "DIR")
    split-commas)

  (add-option! parser
    "module"
    (abbreviation "m")
    (help "Specify the module to compile source file in")
    (value-help "MODULE-NAME"))

  (add-flag! parser
    "help"
    not-negatable
    (help "Show this help message and exit"))
  (add-flag! parser
    "help-flags"
    (aliases '("dump-flags"))
    not-negatable
    (help "List every flag (env var and CLI alias) with current value and exit"))

  (run))
