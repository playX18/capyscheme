(import (rnrs) (srfi 64))
;; flmin, flmax and flabs in the optimizing tier, against a reference
;; written with comparisons only. Each check runs 20000 times so the
;; procedure reaches Tier 2 and its inline lowering is what is tested. NaN
;; operands and signed zeros are part of the input set: flmin/flmax return
;; NaN when either operand is NaN, and order -0.0 below +0.0.
(test-begin "opt flonum min/max/abs")
(define failures '())
(define (same? a b)
  (or (and (flonum? a) (flonum? b) (flnan? a) (flnan? b))
      (eqv? a b)))
(define (check name got want)
  (unless (or (same? got want) (assq name failures))
    (set! failures (cons (list name got want) failures))))
(define-syntax raises?
  (syntax-rules () [(_ e) (guard (c [#t #t]) e #f)]))

(define nan (fl/ 0.0 0.0))
(define values* (vector -inf.0 -1.5 -0.0 0.0 2.25 +inf.0 nan 1e-300 -7.0))

(define (negative-zero? x) (and (fl=? x 0.0) (fl<? (fl/ 1.0 x) 0.0)))

(define (ref-min a b)
  (cond [(or (flnan? a) (flnan? b)) nan]
        [(fl<? a b) a]
        [(fl<? b a) b]
        [(negative-zero? a) a]
        [else b]))
(define (ref-max a b)
  (cond [(or (flnan? a) (flnan? b)) nan]
        [(fl>? a b) a]
        [(fl>? b a) b]
        [(negative-zero? a) b]
        [else a]))
(define (ref-abs a)
  (cond [(flnan? a) a]
        [(fl<? a 0.0) (fl- 0.0 a)]
        [else (fl+ a 0.0)]))

(define (run i)
  (let ([a (vector-ref values* (mod i (vector-length values*)))]
        [b (vector-ref values* (mod (div i (vector-length values*)) (vector-length values*)))])
    (check 'flmin (flmin a b) (ref-min a b))
    (check 'flmax (flmax a b) (ref-max a b))
    (check 'flabs (flabs a) (ref-abs a))
    ;; Chained, as in a slab test: the results stay unboxed in between.
    (check 'chain (flmax (flmin a b) (flmin (flabs a) b))
      (ref-max (ref-min a b) (ref-min (ref-abs a) b)))
    ;; A non-flonum operand deopts and raises like the procedure.
    (check 'min-type (raises? (flmin 1 b)) #t)
    (check 'abs-type (raises? (flabs (if (odd? i) 'x 2))) #t)))

(do ([i 0 (+ i 1)]) ((= i 20000)) (run i))

(for-each (lambda (f) (display "mismatch: ") (write f) (newline)) failures)
(test-equal "no mismatches" '() failures)
(test-end)
