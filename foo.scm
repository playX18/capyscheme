(library (foo)
  (export fac)
  (import (rnrs))
(define (fac n)
  (if (= n 0)
      1
      (* n (fac (- n 1)))))
)