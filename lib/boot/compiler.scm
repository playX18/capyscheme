(|#%require| (boot prim) (boot control) (boot records) (boot conditions) (boot violations) (boot raise) (boot exceptions) (boot expand) (boot interpreter) (boot enums) (boot sys) (boot osdep) (boot ports-core) (boot transcoder) (boot binary-ports) (boot textual-ports) (boot custom-ports) (boot file-ports) (boot console-ports) (boot port-api) (boot print) (boot format) (boot log) (boot match-syntax) (boot units) (boot psyntax-exp) (boot str2num) (boot num2str) (boot reader) (boot environments) (boot eval) (boot namespace) (boot time) (boot base) (boot match))
(|#%provide| all-defined)


(import
  (capy compiler tree-il fix-letrec)
  (capy compiler tree-il assignment-elimination)
  (capy compiler tree-il well-known-procs)
  (capy compiler tree-il primitives)
  (capy compiler tree-il terms)
  (capy pretty-print)
  (capy compiler tree-il))

;; Self-hosted compiler which utilizes code written in Scheme
;; to compile & optimize Scheme.
(define (%runtime-stats-timed-reader thunk)
  (let ([token #f])
    (dynamic-wind
      (lambda ()
        (set! token (%runtime-stats-begin-reader)))
      thunk
      (lambda ()
        (%runtime-stats-end-reader token)))))

;; Per-pass timing for the Scheme-side compile pipeline. Enabled by the same
;; CAPY_PROFILE_PASSES env var as the Rust pass profiler (utils/pass_profile.rs).
(define (%profile-phase name thunk)
  (let ([flag (getenv "CAPY_PROFILE_PASSES")])
    (if (and flag (not (equal? flag "")))
      (let ([t0 (microsecond)])
        (call-with-values
          thunk
          (lambda results
            (let ([t1 (microsecond)])
              (display
                (string-append
                  ";; PERF (capy) phase=" name " elapsed_ms="
                  (number->string (quotient (- t1 t0) 1000))
                  "\n")
                (current-error-port))
              (apply values results)))))
      (thunk))))

(%%file-compiler
  (lambda (filename compiled-path env load-thunk? . maybe-dump-options)
    (define dump-options
      (if (null? maybe-dump-options) '() (car maybe-dump-options)))
    (define (read-all in)
      (%profile-phase
        "scheme.read_all"
        (lambda ()
          (let lp ([exps '()])
            (let ([exp (%runtime-stats-timed-reader (lambda () (read-syntax in)))])
              (cond
                [(eof-object? exp) (reverse exps)]
                [else (lp (cons exp exps))]))))))
    (define output-file (or compiled-path (compiled-file-name filename)))
    (define module (or env (current-namespace) (expansion-env)))
    (*raw-log* log:debug
      '(capy)
      'compile-file
      "Compiling file ~a to ~a"
      filename
      output-file)
    (dynamic-wind
      (lambda ()
        (%runtime-stats-begin-compilation))
      (lambda ()
        (call-with-input-file filename
          (lambda (in)
            (define exps (read-all in))
            (define reader (get-port-reader in #f))
            (with-continuation-mark *compile-backtrace-key* (not (reader-nobacktrace? reader))
              (receive (code mod new-mod)
                (%profile-phase "scheme.compile_tree_il" (lambda () (compile-tree-il exps module filename)))
                (let* ([code (%profile-phase "scheme.resolve_primitives"
                               (lambda () (resolve-primitives code #f)))]
                       [code (%profile-phase "scheme.expand_primitives"
                               (lambda () (expand-primitives code)))]
                       [code (%profile-phase "scheme.fix_letrec"
                               (lambda () (fix-letrec code)))]
                       [code (%profile-phase "scheme.expand_well_known_procs"
                               (lambda () (expand-well-known-procs code)))]
                       [code (%profile-phase "scheme.assignment_elimination"
                               (lambda () (eliminate-assignments code)))])
                  (%compile code output-file load-thunk? dump-options #t)))))))
      (lambda ()
        (%runtime-stats-end-compilation)))))
