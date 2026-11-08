(library (boot cli)
  (export enter eval-string eval-string/value enter-compiler)
  (import (capy)
    (core control)
    (core repl)
    (capy args))

  (define (lib-ref key name)
    (unless (library-instance (current-library-registry) key)
      (resolve-env key))
    (instance-ref (library-instance (current-library-registry) key) name))

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
          (define res (parse-args parser (cdr args)))

          (when (arg-results-ref res "version")
            (format #t "CapyScheme ~a~%" (implementation-version))
            (exit 0))

          (let ([log-level (arg-results-ref res "log-level")])
            (cond
              [(equal? log-level "trace")
                (log:set-max-level! log:trace)
                (gc-logging-enable! #t)]
              [(equal? log-level "debug") (log:set-max-level! log:debug)]
              [(equal? log-level "info") (log:set-max-level! log:info)]
              [(equal? log-level "warn") (log:set-max-level! log:warn)]
              [(equal? log-level "error") (log:set-max-level! log:error)]
              [else
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
                    (gc-logging-enable! #t)))]))
          (if (not (zero? (log:max-level)))
            (log:set-logger! *simple-logger*))
          (when (arg-results-ref res "help")
            (format #t "CapyScheme ~a~%" (implementation-version))
            (format #t "Usage: capy [options] [--] [file.scm [args ...]]~%~a~%" (argparser-usage parser))
            (format #t "Examples:~%")
            (format #t "  capy script.scm arg ...    Run script.scm with arguments~%")
            (format #t "  capy -c '(display \"hi\")'   Evaluate an expression and exit~%")
            (format #t "  capy -L lib -l init.scm    Load init.scm with an extra load path~%")
            (format #t "Run `capy --help-flags` for the full list of runtime flags (environment variables).~%")
            (exit 0))
          (when (arg-results-ref res "entrypoint")
            (set! entrypoint (parse-entrypoint (arg-results-ref res "entrypoint"))))
          (when (and (arg-results-ref res "r7rs") (arg-results-ref res "r6rs"))
            (cli-error "Cannot specify both --r7rs and --r6rs modes"))
          (cond
            [(arg-results-ref res "r7rs") (install-r7rs!)]
            [(arg-results-ref res "r6rs") (install-r6rs!)]
            [else (install-compat!)])
          (set! %load-path (append (canonicalize-paths (reverse (arg-results-ref res "load-path"))) %load-path))
          (set! %load-path (append %load-path (canonicalize-paths (reverse (arg-results-ref res "append-load-path")))))
          (set! %load-compiled-path (append (canonicalize-paths (reverse (arg-results-ref res "compiled-load-path"))) %load-compiled-path))
          (set! %load-extensions (append (reverse (arg-results-ref res "extensions")) %load-extensions))
          (install-core-in-user-module!)
          (if (arg-results-ref res "fresh-auto-compile")
            (set! %fresh-auto-compile #t))
          (if (arg-results-ref res "debug")
            (load-debug-enable! #t))
          (define out '())

          (when (arg-results-ref res "runtime-stats")
            (set! out (cons `(,runtime-stats-enable! #t) out)))

          (when (arg-results-ref res "load")
            (set! out (cons `(,load ,(arg-results-ref res "load")) out)))
          (when (arg-results-ref res "script")
            (set! arg0 (arg-results-ref res "script"))
            (set! interactive? #f)
            (set! out (cons `(,load-program ,(arg-results-ref res "script")) out)))
          (when (arg-results-ref res "command")
            (set! interactive? #f)
            (set! out (cons `(,eval-string ,(arg-results-ref res "command")) out)))
          (when (pair? (arg-results-ref res "doc-module"))
            (set! interactive? #f)
            (set! out (cons `(,(lib-ref '(capy docgen) 'docgen-main)
                              (list ,@(arg-results-ref res "doc-module"))
                              ,(arg-results-ref res "doc-output"))
                            out)))
          ;; `capy file.scm args...`: run the first positional as a script, the
          ;; rest stay available as program arguments.
          (let ([files (arg-results-rest res)])
            (when (and interactive? (pair? files))
              (set! interactive? #f)
              (set! out (cons `(,load-program ,(car files)) out))))
          (when entrypoint
            (set! interactive? #f)
            (set! out (cons `(,entrypoint) out)))
          (when (and (arg-results-ref res "runtime-stats") (not interactive?))
            (set! out (cons `(,format #t "~a~%" (,runtime-stats-report)) out)))
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
      (abbreviation "h")
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
    (argparser-add-separator! parser "Logging options:")
    (add-option! parser
      "log-level"
      (allowed (list "trace" "debug" "info" "warn" "error"))
      (value-help "LEVEL")
      (defaults-to #f)
      (help "Set the log level; overrides the legacy --log-trace, --log-debug, --log-info, --log-warn and --log-error flags"))
    (add-flag! parser
      "log-trace"
      not-negatable
      hide
      (help "Legacy alias, prefer --log-level trace"))
    (add-flag! parser
      "log-info"
      not-negatable
      hide
      (help "Legacy alias, prefer --log-level info"))
    (add-flag! parser
      "log-debug"
      not-negatable
      hide
      (help "Legacy alias, prefer --log-level debug"))
    (add-flag! parser
      "log-warn"
      not-negatable
      hide
      (help "Legacy alias, prefer --log-level warn"))
    (add-flag! parser
      "log-error"
      not-negatable
      hide
      (help "Legacy alias, prefer --log-level error"))
    (add-flag! parser
      "version"
      not-negatable
      (help "Show version and exit"))

    (run)))

(define (enter-compiler args)
  "Entrypoint for `capyc` compiler CLI."
  (define arg0 "capyc")
  (define parser (argparser))

  (define (print-help)
    (format #t "CapyScheme Compiler ~a~%" (implementation-version))
    (format #t "Usage: capyc [options] file ...~%~a~%" (argparser-usage parser))
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
      [r6rs-mode (install-r6rs!)]
      [else (install-compat!)])
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
      (runtime-stats-enable! #t))
    (when (arg-results-ref res "nobacktrace")
      (format (current-error-port)
        "warning: --nobacktrace is deprecated; debug is controlled when FASL is loaded~%"))

    (define boot-expander (arg-results-ref res "boot-expander"))
    (when (and boot-expander (not out-file))
      (cli-error "--boot-expander needs --output"))

    (define exe (arg-results-ref res "exe"))
    (define static (or (arg-results-ref res "static") exe))
    (define no-shebang (arg-results-ref res "no-shebang"))
    (define static-report (arg-results-ref res "static-report"))
    (define static-size (arg-results-ref res "static-size"))
    (define static-why (arg-results-ref res "static-why"))
    (define static-mode (arg-results-ref res "static-mode"))
    (define static-dry-run (arg-results-ref res "static-dry-run"))
    (define static-no-shake (arg-results-ref res "static-no-shake"))
    (define static-dump-tree-il (arg-results-ref res "static-dump-tree-il"))
    (define static-dump-bytecode (arg-results-ref res "static-dump-bytecode"))
    (when (and static (not out-file))
      (cli-error "--static needs --output"))
    (when (and static (not (= (length source-files) 1)))
      (cli-error "--static takes exactly one input file"))
    (when (and static boot-expander)
      (cli-error "--static and --boot-expander cannot be combined"))
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
                (print-condition exn (current-error-port))
                (format (current-error-port) ";; Compilation of file ~a failed.~%" file)
                (when (marks-condition? exn)
                  (stack-trace (condition-marks exn) (current-error-port)))
                (exit 1))
              (lambda ()
                (when verbose
                  (format #t ";; Compiling file ~a~%" file))
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

  (add-option! parser
    "output"
    (abbreviation "o")
    (help "Specify output file"))

  (add-flag! parser
    "boot-expander"
    not-negatable
    (help "Compile with the Rust boot expander, for the boot loader lib/boot/main.scm"))

  (add-flag! parser
    "static"
    not-negatable
    (help "Link the program and everything it depends on into one boot-less fasl (needs --output)"))
  (add-flag! parser
    "exe"
    not-negatable
    (help "With --static, write a program that runs without capy: a copy of the capy launcher with the program appended (macOS: ad-hoc signed). The launcher loads libcapy from its own directory, so the output needs libcapy.so (libcapy.dylib) next to it"))
  (add-flag! parser
    "no-shebang"
    not-negatable
    (help "With --static, do not put a #!/usr/bin/env capy line before the fasl"))
  (add-flag! parser
    "static-report"
    not-negatable
    (help "With --static, print the linklets that were kept and dropped"))
  (add-flag! parser
    "static-size"
    not-negatable
    (help "With --static, print where the bytes of the bundle go: per section of the code blocks, raw and compressed"))

  (add-option! parser
    "static-mode"
    (allowed (list "auto" "open" "closed"))
    (value-help "MODE")
    (help "With --static, link open (keeps the expander), closed (fails if the program needs it) or auto (default)"))
  (add-flag! parser
    "static-no-shake"
    not-negatable
    (help "With --static, keep every binding of the linklets that take part (for bisecting the shaker)"))
  (add-flag! parser
    "static-dry-run"
    not-negatable
    (help "With --static, analyse and report but do not compile or write the output"))
  (add-flag! parser
    "static-dump-tree-il"
    not-negatable
    (help "With --static, write the merged bundle body as Scheme (tree-il->scheme) to OUT.tree-il.scm, and the body after the compiler passes to OUT.tree-il.opt.scm"))
  (add-flag! parser
    "static-dump-bytecode"
    not-negatable
    (help "With --static, dump the compiled bundle bytecode (into the compile dump directory, like --dump-compile bytecode)"))
  (add-option! parser
    "static-why"
    (value-help "NAME")
    (help "With --static, print the chain of bindings that keeps NAME in the bundle"))

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

  (add-flag! parser
    "help"
    (abbreviation "h")
    not-negatable
    (help "Show this help message and exit"))
  (add-flag! parser
    "help-flags"
    (aliases '("dump-flags"))
    not-negatable
    (help "List every flag (env var and CLI alias) with current value and exit"))

  (run))
