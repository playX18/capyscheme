(import (capy keywords))

(define (main)
  (define (fac n #:acc [acc 1])
    (if (zero? n)
        acc
        (fac (- n 1) #:acc (* n acc))))
  (printf "Factorial of 5 is ~a~%" (fac 5 #:acc 6)))

(main)