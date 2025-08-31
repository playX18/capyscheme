(define (plus . args)
    (let loop ([acc (car args)]
               [rest (cdr args)])
      (if (null? rest)
          acc
          (loop (+ acc (car rest)) (cdr rest)))))

(set! + plus)

(let ([x +])
    (x 1 2 3))