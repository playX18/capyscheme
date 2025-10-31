#!r6rs 
(library (crossterm tty)
    (export tty?)
    (import (core foreign) (core foreign-library) (rnrs))

(define isatty 
    (let ([fn (foreign-library-function 
        #f 
        "isatty"
        int 
        `(,int))])
        
    (lambda (port)
        (format #t "port-fd: ~a~%" (port-fd port))
        (= (fn (port-fd port)) 1))))

(define (tty? port)
    (and (port? port)
         (isatty port)))
)