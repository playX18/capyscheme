;; The self-hosted compiler: preprocessed Tree IL -> GCPS ->
;; optimized GCPS -> CFG/SSA -> bytecode -> program datum / FASL.

(library (core compiler pipeline)
  (export compile-preprocessed-il compile-il-to-program compile-il-to-file
          compile-il-to-bytevector compile-il-to-thunk)
  (import (core) (capy)
    (core compiler cps convert)
    (core compiler cps graph)
    (core compiler cps pretty)
    (core compiler cps optimize)
    (core compiler cfg pretty)
    (core compiler bytecode lower)
    (core compiler object))

  (define (profile-phase name thunk)
    (let ([flag (getenv "CAPY_PROFILE_PASSES")])
      (if (and flag (not (equal? flag "")))
        (let ([t0 (microsecond)])
          (call-with-values thunk
            (lambda results
              (let ([t1 (microsecond)])
                (display
                  (string-append ";; PERF (capy) phase=" name " elapsed_ms="
                    (number->string (quotient (- t1 t0) 1000)) "\n")
                  (current-error-port))
                (apply values results)))))
        (thunk))))

  ;; => (values compiled-program gcps gcps-opt ssa)
  (define (lower il dump?)
    (let* ([gp (profile-phase "scheme.gcps.convert" (lambda () (cps-toplevel il)))]
           [gcps (and dump?
                   (let ([g (graph-function-program-graph gp)])
                     (render-graph g (read-term-link g (graph-function-program-root gp)))))]
           [opt (profile-phase "scheme.gcps.optimize"
                  (lambda () (optimize-graph-func-to-ssa gp dump?)))]
           [ssa (optimized-program-ssa opt)]
           [ssa-text (and dump? (string-append (render-program ssa) "\n"))]
           [program (profile-phase "scheme.bytecode" (lambda () (compile-program ssa)))])
      (values program gcps (optimized-program-graph-cps opt) ssa-text)))

  (define (compile-il-to-program il)
    (let-values ([(program gcps gcps-opt ssa) (lower il #f)]) program))

  (define (compile-preprocessed-il il dump?)
    (let-values ([(program gcps gcps-opt ssa) (lower il dump?)])
      (list (cons 'gcps gcps)
            (cons 'gcps-opt gcps-opt)
            (cons 'ssa ssa)
            (cons 'program (program->datum program))
            (cons 'directory (directory-header il)))))

  (define (compile-il-to-file il output-file)
    (write-compiled-program (compile-il-to-program il) il output-file))

  (define (compile-il-to-bytevector il)
    (compiled-program->bytevector (compile-il-to-program il) il))

  (define (compile-il-to-thunk il)
    (load-compiled-program (compile-il-to-program il) il))
)
