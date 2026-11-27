;; Regression: division by exact zero and fx/fl type checks must agree on all
;; engines, before and after JIT warm-up, for literal and non-literal operands.
;; R6RS 11.7.4.3: an inexact dividend over exact 0 is +inf.0/+nan.0 (not an
;; error); an exact dividend over exact 0 is &assertion.
(import (rnrs) (rnrs r5rs))

(define (outcome thunk)
  (guard (e [(assertion-violation? e) 'assertion]
            [(implementation-restriction-violation? e) 'restriction]
            [(error? e) 'error]
            [else 'other])
    (let ([v (thunk)]) (if (number? v) (list 'ok v) (list 'ok-nonnum v)))))

(define failures 0)
(define (check name expected thunk)
  (let ([got (outcome thunk)])
    (unless (equal? got expected)
      (set! failures (+ failures 1))
      (display (list 'FAIL name 'expected expected 'got got))
      (newline))))

(define (nz) (- 1 1))
(define (run x f bad)
  ;; x exact 0 (non-literal), f 0.0 (non-literal), bad a non-number set
  (check '/ '(ok +inf.0) (lambda () (/ 0.5 x)))
  (check '/-neg '(ok -inf.0) (lambda () (/ -0.5 x)))
  (check '/-nan '(ok +nan.0) (lambda () (/ f x)))
  (check '/-exact0 'assertion (lambda () (/ x x)))
  (check '/-int 'assertion (lambda () (/ 5 x)))
  (check '/-lit '(ok +inf.0) (lambda () (/ 0.5 0)))
  (check '/-0.0 '(ok +inf.0) (lambda () (/ 0.5 f)))
  (check '/-0/2.5 '(ok 0.0) (lambda () (/ x 2.5)))
  (check 'div 'assertion (lambda () (div 4.0 x)))
  (check 'mod 'assertion (lambda () (mod 4.0 x)))
  (check 'div0 'assertion (lambda () (div0 4.0 x)))
  (check 'mod0 'assertion (lambda () (mod0 4.0 x)))
  (check 'div-int 'assertion (lambda () (div 5 x)))
  (check 'quotient 'assertion (lambda () (quotient 5. x)))
  (check 'remainder 'assertion (lambda () (remainder 5. x)))
  (check 'modulo 'assertion (lambda () (modulo 5. x)))
  (check 'quotient-fl '(ok 2.0) (lambda () (quotient 5. (+ f 2.))))
  (check 'eis-neg 'assertion (lambda () (exact-integer-sqrt (- x 1))))
  (check 'eis-fl 'assertion (lambda () (exact-integer-sqrt (+ f 4.))))
  (check 'eis-ok '(ok 2) (lambda () (let-values ([(s r) (exact-integer-sqrt (+ x 4))]) s)))
  (check 'fx+ 'assertion (lambda () (fx+ 7 bad)))
  (check 'fx- 'assertion (lambda () (fx- bad 7)))
  (check 'fx* 'assertion (lambda () (fx* 7 bad)))
  (check 'fx<? 'assertion (lambda () (fx<? 7 bad)))
  (check 'fx=? 'assertion (lambda () (fx=? 7 bad)))
  (check 'fxand 'assertion (lambda () (fxand 7 bad)))
  (check 'fxzero? 'assertion (lambda () (fxzero? bad)))
  (check 'fxnot 'assertion (lambda () (fxnot bad)))
  (unless (flonum? bad) (run-fl bad))
  (check 'fx+ok '(ok 9) (lambda () (fx+ 7 (+ x 2))))
  (check 'fl+ok '(ok 3.0) (lambda () (fl+ 1.0 (+ f 2.0)))))

(define (run-fl bad)
  (check 'fl>? 'assertion (lambda () (fl>? 1.0 bad)))
  (check 'fl<? 'assertion (lambda () (fl<? bad 1.0)))
  (check 'fl+ 'assertion (lambda () (fl+ 1.0 bad)))
  (check 'fl* 'assertion (lambda () (fl* bad 1.0)))
  (check 'fl=? 'assertion (lambda () (fl=? 1.0 bad)))
  (check 'flsqrt 'assertion (lambda () (flsqrt bad)))
  (check 'flzero? 'assertion (lambda () (flzero? bad))))

(do ([i 0 (+ i 1)]) ((= i 300))
  (run (nz) (* 1.0 (nz)) (vector 1))
  (run (nz) (* 1.0 (nz)) #t)
  (run (nz) (* 1.0 (nz)) 1.5)
  (run (nz) (* 1.0 (nz)) 'a))

(if (= failures 0) (begin (display "ok") (newline)) (begin (display failures) (newline) (exit 1)))
