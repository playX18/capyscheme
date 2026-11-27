(import (srfi 64) (capy) (srfi 39))

(test-begin "boot parameters")

(test-group "make-parameter converter"
  (define p (make-parameter 10 (lambda (x) (* x 2))))
  (test-equal "initial value is converted" 20 (p))
  (test-equal "parameterize converts the new value" 6 (parameterize ((p 3)) (p)))
  (test-equal "value restored after parameterize" 20 (p))
  (test-equal "re-entry through a continuation does not convert twice"
    6
    (let ((k #f) (n 0))
      (parameterize ((p 3))
        (call/cc (lambda (c) (set! k c)))
        (set! n (+ n 1))
        (if (< n 2) (k #f))
        (p))))
  (test-equal "unconverted parameter" 3
    (let ((q (make-parameter 1))) (parameterize ((q 3)) (q))))
  (test-error "converter must be a procedure" #t (make-parameter 1 2)))

(test-end "boot parameters")
