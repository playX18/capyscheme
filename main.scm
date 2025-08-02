(define (fac nx)
    (let loop ((n nx) (acc 1))
      (display nx)
      (if (= n 0)
          acc
          (loop (- n 1) (* n acc)))))

(fac 5)