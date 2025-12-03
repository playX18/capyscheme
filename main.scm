(import (capy keywords))
(import (capy pretty-print))
(import (capy compiler tree-il))


(define (main)
  (define (fac n #:acc [acc 1])
    (if (= n 0)
        acc
        (fac (- n 1) #:acc (* n acc))))
  
  (printf "Factorial of 5 is ~a\n" (fac 5))
  (printf "Factorial of 6 is: ~a~%" (fac 5 #:acc 6)))

(main)