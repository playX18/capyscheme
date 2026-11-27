(import (rnrs) (srfi 64))
;; fxarithmetic-shift-left and -right with a count that is not a constant: Tier 2 lowers them
;; to register shifts (ShlI32OvfVar, ShiftI32Var). Each check runs 20000 times, so the
;; procedure reaches the optimizing tier; the counts sweep their range, and the out-of-range
;; and overflowing cases must still raise (by deopting to baseline). The first mismatch of
;; every check is kept.
(test-begin "opt variable fixnum shifts")
(define failures '())
(define (check name got want)
  (unless (or (equal? got want) (assq name failures))
    (set! failures (cons (list name got want) failures))))
(define-syntax raises?
  (syntax-rules () [(_ e) (guard (c [#t #t]) e #f)]))

(define (run i)
  (let ([x (fx- (fxand i 4095) 2048)]
        [n (fxand i 15)]
        [m (fxand (fxarithmetic-shift-right i 4) 31)])
    (check 'ashl (fxarithmetic-shift-left x n) (* x (expt 2 n)))
    (check 'ashl-byte (fxarithmetic-shift-left (fxand i 255) (fxand i 7)) (* (fxand i 255) (expt 2 (fxand i 7))))
    (check 'ashr (fxarithmetic-shift-right x n) (div x (expt 2 n)))
    (check 'ashr-wide (fxarithmetic-shift-right (fx* x 100000) m) (div (* x 100000) (expt 2 m)))
    (check 'ashl-ovf (raises? (fxarithmetic-shift-left (fx+ 1073741824 (fxand i 1)) (fx+ 1 (fxand i 1)))) #t)
    (check 'ashl-ovf-neg (raises? (fxarithmetic-shift-left (fx- -1073741825 (fxand i 1)) (fx+ 1 (fxand i 1)))) #t)
    (check 'ashl-count-32 (raises? (fxarithmetic-shift-left 1 (fx+ 32 (fxand i 1)))) #t)
    (check 'ashl-count-neg (raises? (fxarithmetic-shift-left 1 (fx- -1 (fxand i 1)))) #t)
    (check 'ashl-zero-count (fxarithmetic-shift-left x (fxand i 0)) x)
    (check 'ashl-31 (fxarithmetic-shift-left (fxand i 0) (fx+ 31 (fxand i 0))) 0)
    (check 'ashl-min (fxarithmetic-shift-left -1 (fx+ 31 (fxand i 0))) (least-fixnum))))

(do ([i 0 (+ i 1)]) ((= i 20000)) (run i))
(for-each (lambda (f) (display f) (newline)) failures)
(test-equal "no mismatches" '() failures)
(test-end "opt variable fixnum shifts")
