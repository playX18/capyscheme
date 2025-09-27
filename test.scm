
(define-module (bar)
    (export (rename foo base:foo))
    (define (foo) 42)
    (letrec ([x 42]
             [y (lambda () (+ (foo) x))])
        (y)))

(define-module (baz))

(import (prefix (bar) bar:))
(print (bar:base:foo))
