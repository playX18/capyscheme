(define-library (scmline)
    (import)
    (export)
(begin 
    (define (foo x) x)
    (define (bar y) (foo y))
    (define x 42)
    (set! x 1)
))