42

(import (capy compiler tree-il primitives) (capy pretty-print) (capy compiler tree-il))

;; Self-hosted compiler which utilizes code written in Scheme
;; to compile & optimize Scheme.
(%%file-compiler 
  (lambda (filename compiled-path env load-thunk?)
    (define (read-all in)
        (let lp ([exps '()])
            (let ([exp (read-syntax in)])
                (cond
                    [(eof-object? exp) (reverse exps)]
                    [else (lp (cons exp exps))]))))
    (define output-file (or compiled-path (compiled-file-name filename)))
    (define module (or env (resolve-module '(capy user) #f #f)))
    (*raw-log* log:debug
               '(capy)
               'compile-file
               "Compiling file ~a to ~a" filename output-file)
    
    (call-with-input-file filename
        (lambda (in)
            (define exps (read-all in))
            (define reader (get-port-reader in #f))
            (with-continuation-mark *compile-backtrace-key* (not (reader-nobacktrace? reader))
                (receive (code mod new-mod) (compile-tree-il exps module) 
                    (let* ([code (resolve-primitives code mod)]
                           [code (expand-primitives code)])  
                    (%compile code output-file mod load-thunk?)))))))
                    )