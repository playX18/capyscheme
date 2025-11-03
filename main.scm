(define (make-counter n)
  (lambda ()
    (set! n (+ n 1))
    n))