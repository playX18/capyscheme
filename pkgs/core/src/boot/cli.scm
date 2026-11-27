(library (boot cli)
  (export enter eval-string eval-string/value enter-compiler)
  (import (capy)
    (core control)
    (core repl)
    (core flags))


  ;;; Flags

  ;; The flags the two entry points have in common.
  (define view-spec
    (option "view"
      "Resolve library names as the package PACKAGE does: the view of that package in the libmap"
      "PACKAGE"))
  (define output-directory-spec
    (option "compile-output-directory"
      "The directory for the libraries that code generates while it is compiled (c-include), as libraries of the package: capy build gives each package one"
      "DIR"))
  (define load-path-spec
    (multi-option "load-path" "Add a directory to the load path" "DIR" 'abbr "L"))
  (define append-load-path-spec
    (multi-option "append-load-path" "Append a directory to the load path" "DIR" 'abbr "A"))
  (define compiled-load-path-spec
    (multi-option "compiled-load-path" "Add a directory to the compiled load path" "DIR" 'abbr "C"))
  (define extensions-spec
    (multi-option "extensions" "Add specified extension to list of extensions" "EXTENSION" 'abbr "x"))
  (define help-flags-spec
    (flag "help-flags"
      "List every flag (env var and CLI alias) with current value and exit"
      'aliases '("dump-flags")))

  ;; Driver features: tools that live in the standard library and that `capy` exposes as flags.
  ;; Their libraries are looked up when the flag is used (`lib-ref`); the runtime does not
  ;; need them. Everything else in this file is the runtime and the file compiler.
  (define driver-specs
    (list (multi-option "doc-module"
           "Document a library module as JSON and exit (e.g. --doc-module '(core lists)'); repeatable"
           "MODULE")
      (option "doc-output" "Write --doc-module JSON output to FILE instead of stdout" "FILE")))

  (define (driver-actions res)
    (if (pair? (flag-ref res "doc-module"))
      (list `(,(lib-ref '(capy docgen) 'docgen-main)
              (list ,@(flag-ref res "doc-module"))
              ,(flag-ref res "doc-output")))
      '()))

  (define runtime-specs
    (append
      (list (flag "r7rs" "Run in R7RS compatibility mode")
        (flag "r6rs" "Run in R6RS compatibility mode")
        (flag "help" "Show this help message and exit" 'abbr "h")
        help-flags-spec
        (option "script" "Run specified file as a script" "FILE" 'abbr "s")
        (option "command" "Evaluate a command string" "STRING" 'abbr "c"))
      driver-specs
      (list view-spec output-directory-spec
        (option "load" "Load specified file before entering REPL or running script" "FILE" 'abbr "l"))
      (list load-path-spec append-load-path-spec compiled-load-path-spec extensions-spec
        (option "entrypoint" "Specify the entrypoint function to call" "EXPR" 'abbr "e")
        (flag "fresh-auto-compile" "Enable fresh auto compilation of loaded files")
        (flag "runtime-stats" "Enable runtime timing/counter statistics")
        (flag "debug" "Enable debug stack traces for FASL code (including stdlib)")
        (separator "Logging options:")
        (option "log-level"
          "Set the log level; overrides the legacy --log-trace, --log-debug, --log-info, --log-warn and --log-error flags"
          "LEVEL"
          'allowed '("trace" "debug" "info" "warn" "error"))
        (flag "log-trace" "Legacy alias, prefer --log-level trace" 'hidden #t)
        (flag "log-info" "Legacy alias, prefer --log-level info" 'hidden #t)
        (flag "log-debug" "Legacy alias, prefer --log-level debug" 'hidden #t)
        (flag "log-warn" "Legacy alias, prefer --log-level warn" 'hidden #t)
        (flag "log-error" "Legacy alias, prefer --log-level error" 'hidden #t)
        (flag "version" "Show version and exit"))))

  (define compiler-specs
    (append
      (list (option "output" "Specify output file" #f 'abbr "o")
        view-spec
        output-directory-spec
        (flag "boot-expander"
          "Compile with the Rust boot expander, for the boot loader pkgs/core/src/boot/main.scm")
        (option "error-format"
          "How compile errors and warnings are written: human (rendered, with source lines and a summary) or json (one object per diagnostic on each line of the standard error stream, the form the capy driver reads)"
          "FORMAT"
          'allowed '("human" "json"))
        (flag "lint"
          "Warn about code that is probably wrong: calls of known procedures with the wrong number of arguments")
        (flag "wpo"
          "Also write the .wpo file next to the output: the tree-IL of the libraries of the file, which --static merges instead of expanding the source again")
        (flag "static" "Link the program and everything it depends on into one boot-less fasl (needs --output)")
        (flag "exe"
          "With --static, write a program that runs without capy: a copy of the capy launcher with the program appended (macOS: ad-hoc signed). The launcher loads libcapy from its own directory, so the output needs libcapy.so (libcapy.dylib) next to it")
        (flag "static-native"
          "With --static --exe, link the native extensions the program loads into the executable instead of copying their shared libraries beside it. Needs the static form of the extensions (capy build --static-native), libcapy_static.a next to the launcher, and a C compiler (cc, or $CC) to link with")
        (flag "no-shebang" "With --static, do not put a #!/usr/bin/env capy line before the fasl")
        (flag "static-report" "With --static, print the linklets that were kept and dropped")
        (flag "static-size"
          "With --static, print where the bytes of the bundle go: per section of the code blocks, raw and compressed")
        (option "static-mode"
          "With --static, link open (keeps the expander), closed (fails if the program needs it) or auto (default)"
          "MODE"
          'allowed '("auto" "open" "closed"))
        (flag "static-no-shake"
          "With --static, keep every binding of the linklets that take part (for bisecting the shaker)")
        (flag "static-dry-run" "With --static, analyse and report but do not compile or write the output")
        (flag "static-dump-tree-il"
          "With --static, write the merged bundle body as Scheme (tree-il->scheme) to OUT.tree-il.scm, and the body after the compiler passes to OUT.tree-il.opt.scm")
        (flag "static-dump-bytecode"
          "With --static, dump the compiled bundle bytecode (into the compile dump directory, like --dump-compile bytecode)")
        (option "static-why" "With --static, print the chain of bindings that keeps NAME in the bundle" "NAME")
        (flag "nobacktrace" "Deprecated compatibility flag; debug is controlled when FASL is loaded")
        (flag "r7rs" "Compile in R7RS mode")
        (flag "r6rs" "Compile in R6RS mode")
        (flag "verbose" "Enable verbose output" 'abbr "v")
        (option "barrier"
          "Force FASL write-barrier kind: nobarrier, objbarrier, or satbbarrier (default: from live --gc-plan)"
          "KIND")
        (flag "version" "Show version and exit")
        (flag "runtime-stats" "Enable runtime timing/counter statistics")
        (separator "Compiler dump options:")
        (flag "dump-graph" "Dump graph CPS for each compiled file")
        (flag "dump-ssa" "Dump SSA for each compiled file")
        (flag "dump-cranelift" "Dump Cranelift IR for each compiled file")
        (flag "dump-disassembly" "Dump native-code disassembly for each compiled file"))
      (list load-path-spec compiled-load-path-spec extensions-spec append-load-path-spec
        (flag "help" "Show this help message and exit" 'abbr "h")
        help-flags-spec)))

  (define (lib-ref key name)
    (unless (library-instance (current-library-registry) key)
      (resolve-env key))
    (instance-ref (library-instance (current-library-registry) key) name))

  ;; Writes the diagnostics DS of the compilation of FILE to PORT: rendered with their source
  ;; lines and a closing summary, or as JSON, one object on a line. The renderer lives in std,
  ;; which core cannot import, so it is looked up with `lib-ref`; raises when it is not there.
  (define (write-diagnostics ds file port json?)
    (let ([severity (lib-ref '(core diagnostics) 'diagnostic-severity)])
      (cond
        [json?
          (let ([->json (lib-ref '(capy diagnostics json) 'diagnostic->json-string)])
            (for-each
              (lambda (d)
                (put-string port (->json d))
                (newline port))
              ds)
            (flush-output-port port))]
        [else
          (let ([render (lib-ref '(capy diagnostics render) 'render-diagnostic)]
                [summary (lib-ref '(capy diagnostics render) 'render-diagnostics-summary)]
                [make-painter (lib-ref '(capy term paint) 'make-painter)]
                [count (lambda (kind)
                         (length (filter (lambda (d) (eq? (severity d) kind)) ds)))])
            (let ([painter (make-painter 'auto port)])
              (for-each (lambda (d) (render d painter port)) ds)
              (summary (string-append "`" file "`") (count 'error) (count 'warning)
                painter port)))])))

  ;; Prints EXN, raised while compiling FILE, as diagnostics: all the errors of a
  ;; `&compilation-errors`, or the one error of any other condition that has a position. Returns
  ;; #f, having printed nothing, when the diagnostics cannot be written or EXN has no position,
  ;; and the caller falls back to `print-condition`.
  (define (render-compile-error exn file port json?)
    (call/cc
      (lambda (return)
        (with-exception-handler
          (lambda (e) (return #f))
          (lambda ()
            (let* ([->diagnostic (lib-ref '(core diagnostics) 'condition->diagnostic)]
                   [primary (lib-ref '(core diagnostics) 'diagnostic-primary)]
                   [collected? (compilation-errors-condition? exn)]
                   [ds (if collected?
                         (condition-compilation-diagnostics exn)
                         (list (->diagnostic exn)))])
              (and (pair? ds)
                (or collected? (primary (car ds)))
                (begin
                  (write-diagnostics ds file port json?)
                  #t))))))))

  ;; Called with the warnings a compilation of FILE reported and that did not fail it.
  (define (report-warnings ds file port json?)
    (call/cc
      (lambda (return)
        (with-exception-handler
          (lambda (e) (return #f))
          (lambda () (write-diagnostics ds file port json?) #t)))))

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
              (lp (eval exp (current-namespace)))
              result))))))

  (define (eval-string str)
    (eval-string/value str))


  ;; Makes the view of the package PACKAGE (a name) current: the names of libraries are the ones
  ;; the package writes, see `linker-view-key`. The view is in the libmap.
  (define (install-view! package)
    (when package
      (let ([view ((lib-ref '(boot linker) 'linker-libmap-view) (string->symbol package))])
        (unless view
          (cli-error "error: no view of package ~a in the libmap (--libmap)~%" package))
        ((lib-ref '(boot environments) 'current-library-view) view))))

  ;; Makes DIR the directory generated libraries are written to, `(compile-output-directory)`.
  (define (install-output-directory! dir)
    (when dir
      ((lib-ref '(boot environments) 'compile-output-directory) dir)))

  (define (install-core-in-user-module!)
    (namespace-import! (current-namespace) '(core)))

  (define (canonicalize-paths paths)
    (map canonicalize-path-string paths))

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

    (define specs runtime-specs)

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
                    (list read-eval-print-loop)
                    '(exit 0)))))))
    (define (run)
      (with-exception-handler
        (lambda (c)
          (let ([message (if (message-condition? c) (condition-message c) c)])
            (if (who-condition? c)
              (format (current-error-port) "Failed to parse command line arguments (~a): ~a~%"
                (condition-who c)
                message)
              (format (current-error-port) "Failed to parse command line arguments: ~a~%"
                message)))
          (exit 1))
        (lambda ()
          (define res (parse-flags specs (cdr args)))

          (when (flag-ref res "version")
            (format #t "CapyScheme ~a~%" (implementation-version))
            (exit 0))

          (let ([log-level (flag-ref res "log-level")])
            (cond
              [(equal? log-level "trace")
                (log:set-max-level! log:trace)
                (gc-logging-enable! #t)]
              [(equal? log-level "debug") (log:set-max-level! log:debug)]
              [(equal? log-level "info") (log:set-max-level! log:info)]
              [(equal? log-level "warn") (log:set-max-level! log:warn)]
              [(equal? log-level "error") (log:set-max-level! log:error)]
              [else
                (if (flag-ref res "log-warn")
                  (log:set-max-level! log:warn))
                (if (flag-ref res "log-error")
                  (log:set-max-level! log:error))
                (if (flag-ref res "log-info")
                  (log:set-max-level! log:info))
                (if (flag-ref res "log-debug")
                  (log:set-max-level! log:debug))
                (if (flag-ref res "log-trace")
                  (begin
                    (log:set-max-level! log:trace)
                    (gc-logging-enable! #t)))]))
          (if (not (zero? (log:max-level)))
            (log:set-logger! *simple-logger*))
          (when (flag-ref res "help")
            (format #t "CapyScheme ~a~%" (implementation-version))
            (format #t "Usage: ~a [options] [--] [file.scm [args ...]]~%~a~%"
              (program-name (car args) "capy") (flags-usage specs))
            (format #t "Examples:~%")
            (format #t "  capy script.scm arg ...    Run script.scm with arguments~%")
            (format #t "  capy -c '(display \"hi\")'   Evaluate an expression and exit~%")
            (format #t "  capy -L ./mylibs -l init.scm   Load init.scm with an extra load path~%")
            (format #t "Run `capy --help-flags` for the full list of runtime flags (environment variables).~%")
            (exit 0))
          (when (flag-ref res "entrypoint")
            (set! entrypoint (parse-entrypoint (flag-ref res "entrypoint"))))
          (when (and (flag-ref res "r7rs") (flag-ref res "r6rs"))
            (cli-error "Cannot specify both --r7rs and --r6rs modes"))
          (cond
            [(flag-ref res "r7rs") (install-r7rs!)]
            [(flag-ref res "r6rs") (install-r6rs!)]
            [else (install-compat!)])
          (set! %load-path (append (canonicalize-paths (reverse (flag-ref res "load-path"))) %load-path))
          (set! %load-path (append %load-path (canonicalize-paths (reverse (flag-ref res "append-load-path")))))
          (set! %load-compiled-path (append (canonicalize-paths (reverse (flag-ref res "compiled-load-path"))) %load-compiled-path))
          (set! %load-extensions (append (reverse (flag-ref res "extensions")) %load-extensions))
          (install-core-in-user-module!)
          (install-view! (flag-ref res "view"))
          (install-output-directory! (flag-ref res "compile-output-directory"))
          (if (flag-ref res "fresh-auto-compile")
            (set! %fresh-auto-compile #t))
          (if (flag-ref res "debug")
            (load-debug-enable! #t))
          (define out '())

          (when (flag-ref res "runtime-stats")
            (set! out (cons `(,runtime-stats-enable! #t) out)))

          (when (flag-ref res "load")
            (set! out (cons `(,load ,(flag-ref res "load")) out)))
          (when (flag-ref res "script")
            (set! arg0 (flag-ref res "script"))
            (set! interactive? #f)
            (set! out (cons `(,load-program ,(flag-ref res "script")) out)))
          (when (flag-ref res "command")
            (set! interactive? #f)
            (set! out (cons `(,eval-string ,(flag-ref res "command")) out)))
          (let ([actions (driver-actions res)])
            (when (pair? actions)
              (set! interactive? #f)
              (set! out (append (reverse actions) out))))
          ;; `capy file.scm args...`: run the first positional as a script, the
          ;; rest stay available as program arguments.
          (let ([files (flags-rest res)])
            (when (and interactive? (pair? files))
              (set! interactive? #f)
              (set! out (cons `(,load-program ,(car files)) out))))
          (when entrypoint
            (set! interactive? #f)
            (set! out (cons `(,entrypoint) out)))
          (when (and (flag-ref res "runtime-stats") (not interactive?))
            (set! out (cons `(,format #t "~a~%" (,runtime-stats-report)) out)))
          (finish (flags-rest res) out))))

    (run)))

;; The name the program was run as: the last component of ARGV0, its first argument, or
;; DEFAULT when that is empty. `capy`, `capy-rt` and `capyc` are one executable.
(define (program-name argv0 default)
  (let loop ([i (string-length argv0)])
    (cond
      [(zero? i) (if (string=? argv0 "") default argv0)]
      [(char=? (string-ref argv0 (- i 1)) #\/)
        (if (= i (string-length argv0)) default (substring argv0 i (string-length argv0)))]
      [else (loop (- i 1))])))

(define (enter-compiler args)
  "Entrypoint for `capyc` compiler CLI."
  (define arg0 "capyc")
  (define specs compiler-specs)

  (define (print-help)
    (format #t "CapyScheme Compiler ~a~%" (implementation-version))
    (format #t "Usage: ~a [options] file ...~%~a~%" (program-name (car args) "capyc") (flags-usage specs))
    (format #t "Examples:~%")
    (format #t "  capyc -o prog.fasl prog.scm             Compile prog.scm to prog.fasl~%")
    (format #t "  capyc --static --output prog prog.scm   Link prog.scm and its dependencies into one file~%")
    (format #t "Run `capyc --help-flags` for the full list of runtime flags (environment variables).~%")
    (exit 0))

  (define (run)
    (define res (with-exception-handler
                 (lambda (c)
                   (format (current-error-port) "Failed to parse command line arguments: ~a~%" (condition-message c))
                   (exit 1))
                 (lambda ()
                   (parse-flags specs (cdr args)))))

    (define load-path (flag-ref res "load-path"))
    (define append-load-path (flag-ref res "append-load-path"))
    (define compiled-load-path (flag-ref res "compiled-load-path"))
    (define extensions (flag-ref res "extensions"))
    (define out-file (flag-ref res "output"))
    (define r7rs-mode (flag-ref res "r7rs"))
    (define r6rs-mode (flag-ref res "r6rs"))
    (define verbose (flag-ref res "verbose"))
    (define runtime-stats (flag-ref res "runtime-stats"))
    (define version (flag-ref res "version"))
    (define barrier-kind (flag-ref res "barrier"))
    (define backtrace #t)
    (define dump-options
      (let ([options '()])
        (when (flag-ref res "dump-graph")
          (set! options (cons 'graph options)))
        (when (flag-ref res "dump-ssa")
          (set! options (cons 'ssa options)))
        (when (flag-ref res "dump-cranelift")
          (set! options (cons 'cranelift options)))
        (when (flag-ref res "dump-disassembly")
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
      [r6rs-mode (install-r6rs!)]
      [else (install-compat!)])
    (set! %load-path (append (canonicalize-paths load-path) %load-path))
    (set! %load-path (append %load-path (canonicalize-paths append-load-path)))
    (set! %load-compiled-path (append (canonicalize-paths (reverse compiled-load-path)) %load-compiled-path))
    (set! %load-extensions (append (reverse extensions) %load-extensions))
    (install-core-in-user-module!)
    (install-view! (flag-ref res "view"))
          (install-output-directory! (flag-ref res "compile-output-directory"))
    (define source-files (flags-rest res))
    (if (null? source-files)
      (print-help))

    (when (flag-ref res "help")
      (print-help))

    (when runtime-stats
      (runtime-stats-enable! #t))
    (when (flag-ref res "nobacktrace")
      (format (current-error-port)
        "warning: --nobacktrace is deprecated; debug is controlled when FASL is loaded~%"))

    (define boot-expander (flag-ref res "boot-expander"))
    (when (and boot-expander (not out-file))
      (cli-error "--boot-expander needs --output"))

    (define exe (flag-ref res "exe"))
    (define static (or (flag-ref res "static") exe))
    (define static-native (flag-ref res "static-native"))
    (define no-shebang (flag-ref res "no-shebang"))
    (define static-report (flag-ref res "static-report"))
    (define static-size (flag-ref res "static-size"))
    (define static-why (flag-ref res "static-why"))
    (define static-mode (flag-ref res "static-mode"))
    (define static-dry-run (flag-ref res "static-dry-run"))
    (define static-no-shake (flag-ref res "static-no-shake"))
    (define static-dump-tree-il (flag-ref res "static-dump-tree-il"))
    (define static-dump-bytecode (flag-ref res "static-dump-bytecode"))
    (define wpo (flag-ref res "wpo"))
    (define json-errors? (equal? (flag-ref res "error-format") "json"))
    (when (flag-ref res "lint")
      ((lib-ref '(boot units) 'lint-calls?) #t))
    (when (and wpo (or static boot-expander))
      (cli-error "--wpo cannot be combined with --static or --boot-expander"))
    (when wpo
      ((lib-ref '(boot eval) 'compile-wpo?) #t))
    (when (and static (not out-file))
      (cli-error "--static needs --output"))
    (when (and static (not (= (length source-files) 1)))
      (cli-error "--static takes exactly one input file"))
    (when (and static boot-expander)
      (cli-error "--static and --boot-expander cannot be combined"))
    (when (and static-native (not exe))
      (cli-error "--static-native needs --static --exe"))
    (when (and (or no-shebang static-report static-size static-why static-mode static-dry-run static-no-shake static-dump-tree-il static-dump-bytecode) (not static))
      (cli-error "--no-shebang, --static-report, --static-size, --static-why, --static-dump-tree-il and --static-dump-bytecode need --static"))

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
                (unless (render-compile-error exn file (current-error-port) json-errors?)
                  (print-condition exn (current-error-port))
                  (format (current-error-port) ";; Compilation of file ~a failed.~%" file)
                  (when (marks-condition? exn)
                    (stack-trace (condition-marks exn) (current-error-port))))
                (exit 1))
              (lambda ()
                (when verbose
                  (format #t ";; Compiling file ~a~%" file))
                ((lib-ref '(boot eval) 'compile-warning-handler)
                  (lambda (ds) (report-warnings ds file (current-error-port) json-errors?)))
                (with-continuation-mark *compile-backtrace-key* backtrace
                  (define out
                    (cond
                      [static
                        ((lib-ref '(capy compiler static) 'static-link)
                          file out-file
                          (append
                            (if no-shebang '(no-shebang) '())
                            (if static-report '(report) '())
                            (if static-size '(size) '())
                            (if static-why (list (cons 'why static-why)) '())
                            (cond
                              [(equal? static-mode "open") '(open)]
                              [(equal? static-mode "closed") '(closed)]
                              [else '()])
                            (if static-dry-run '(dry-run) '())
                            (if static-no-shake '(no-shake) '())
                            (if static-dump-tree-il '(tree-il) '())
                            (if static-dump-bytecode '(bytecode) '())
                            (if exe '(exe) '())
                            (if static-native '(static-native) '())
                            (if verbose '(verbose) '())))
                        out-file]
                      [boot-expander
                        (%compile-boot-file file out-file)
                        out-file]
                      [else
                        (compile-file
                        file
                        out-file
                        ; A boot file (it has a `|#%require|` header) is expanded in the core
                        ; environment, like the loader expands it when it boots.
                        (if (%boot-header-requires file) the-core-env (current-namespace))
                        #f
                        dump-options)]))
                  (when verbose
                    (format #t ";; Compiled ~a -> ~a~%" file out))))))
          source-files)
        (when runtime-stats
          (format #t "~a~%" (runtime-stats-report))))))

  (run))
