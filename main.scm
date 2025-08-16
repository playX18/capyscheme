(define (main)
    (define (map f lst)
        (let loop ((l lst) (acc '()))
            (if (null? l)
                    (reverse acc)
                    (loop (cdr l) (cons (f (car l)) acc)))))

    (define (fac-iter n acc)
        (if (zero? n)
            acc 
            (fac-iter (- n 1) (* acc n))))
    (fac-iter 5 1)
)