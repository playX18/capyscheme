(define (fib n)
  (let loop ((a 0) (b 1) (count n))
    (if (= count 0)
        a
        (loop b (+ a b) (- count 1)))))

(printf "~a~%" (fib 10000))