(library (boot cli)
    (export enter eval-string)
    (import (capy)
            (core repl))
    

(define (eval-string str)
    (call-with-input-string 
        str 
        (lambda (in)
            (define reader (make-reader in "stdin"))
            (let lp () 
                (let ([exp (read-datum reader)])
                    (if (not (eof-object? exp))
                        (begin 
                            (eval exp (current-module))
                            (lp))))))))

(define (enter args)
    "Main entrypoint of CapyScheme CLI."

    (format #t "args: ~a~%" args)
    ((@ (core repl) read-eval-print-loop))))