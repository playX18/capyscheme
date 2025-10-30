(define (fac x)
    (if (< x 2)
        1
        (* x (fac (- x 1)))))

(define x (fac 4000))

(format #t "fac(4000) = ~a~%" x)