(define (main)
    (define (foo) (values 1 2 3))
    (receive (x y . z) (foo) x)
)

(receive (x y z) (bar) x)