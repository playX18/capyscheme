
(import
  (capy compiler tree-il resolve-free-vars)
  (capy compiler tree-il letrectify)
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
        (set! token ((@@ (capy) %runtime-stats-begin-reader))))
      thunk
      (lambda ()
        ((@@ (capy) %runtime-stats-end-reader) token)))))

;; Per-pass timing for the Scheme-side compile pipeline. Enabled by the same
;; CAPY_PROFILE_PASSES env var as the Rust pass profiler (utils/pass_profile.rs).
(define (%profile-phase name thunk)
  (let ([flag ((@@ (capy) getenv) "CAPY_PROFILE_PASSES")])
    (if (and flag (not (equal? flag "")))
      (let ([t0 ((@@ (capy) microsecond))])
        (call-with-values
          thunk
          (lambda results
            (let ([t1 ((@@ (capy) microsecond))])
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
    (define module (or env (resolve-module '(capy user) #f #f)))
    (*raw-log* log:debug
      '(capy)
      'compile-file
      "Compiling file ~a to ~a"
      filename
      output-file)
    (dynamic-wind
      (lambda ()
        ((@@ (capy) %runtime-stats-begin-compilation)))
      (lambda ()
        (call-with-input-file filename
          (lambda (in)
            (define exps (read-all in))
            (define reader (get-port-reader in #f))
            (with-continuation-mark *compile-backtrace-key* (not (reader-nobacktrace? reader))
              (receive (code mod new-mod)
                (%profile-phase "scheme.compile_tree_il" (lambda () (compile-tree-il exps module)))
                (let* ([code (%profile-phase "scheme.resolve_primitives"
                               (lambda () (resolve-primitives code mod)))]
                       [code (%profile-phase "scheme.expand_primitives"
                               (lambda () (expand-primitives code)))]
                       [code (%profile-phase "scheme.resolve_free_vars"
                               (lambda () (resolve-free-vars code)))]
                       [code (%profile-phase "scheme.letrectify"
                               (lambda () (letrectify code #t)))]
                       [code (%profile-phase "scheme.fix_letrec"
                               (lambda () (fix-letrec code)))]
                       [code (%profile-phase "scheme.expand_well_known_procs"
                               (lambda () (expand-well-known-procs code)))]
                       [code (%profile-phase "scheme.assignment_elimination"
                               (lambda () (eliminate-assignments code)))])
                  (%compile code output-file mod load-thunk? dump-options #t)))))))
      (lambda ()
        ((@@ (capy) %runtime-stats-end-compilation))))))
