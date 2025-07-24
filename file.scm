(define (main)
    (define (fac-rec n acc)
        (if (= n 0)
            acc
            (fac-rec (- n 1) (* n acc))))
    (fac-rec 5 1))


