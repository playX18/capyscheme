(import (srfi 64)
        (capy))

(test-begin "boot class accessor fast path")

(define-class <fp-base> ()
  ((x #:init-value 1 #:accessor fp-x)))
(define-class <fp-sub> (<fp-base>)
  ((y #:init-value 2 #:accessor fp-y)))
;; Same slot name at a different index than in <fp-base>.
(define-class <fp-rev> (<fp-base>)
  ((z #:init-value 3)
   (x #:init-value 10)))

(define (touch-all)
  (list (fp-x (make <fp-base>)) (fp-x (make <fp-sub>)) (fp-x (make <fp-rev>))))

(test-group "readers and writers"
  (define b (make <fp-base>))
  (define s (make <fp-sub>))
  ;; Run repeatedly so the per-class slot cache is filled and hit.
  (do ((i 0 (+ i 1))) ((= i 5))
    (test-equal (list 1 1 1) (touch-all)))
  (do ((i 0 (+ i 1))) ((= i 3))
    (set! (fp-x b) (+ i 100))
    (test-equal (+ i 100) (fp-x b)))
  (test-equal 1 (fp-x s))
  (test-equal 2 (fp-y s)))

(test-group "redefining the method invalidates the cache"
  (define b (make <fp-base>))
  (do ((i 0 (+ i 1))) ((= i 3)) (fp-x b))
  (define-method (fp-x (o <fp-base>)) 'overridden)
  (test-equal 'overridden (fp-x b)))

(test-group "wrong arity and unbound slots still raise"
  (define-class <fp-unbound> () ((u #:accessor fp-u)))
  (define o (make <fp-unbound>))
  (test-error (fp-u o))
  (test-error (fp-x)))

;; The inline setter logs a not-yet-logged (old) instance out of line, then
;; stores; the young values must survive collections.
(define-class <fp-box> () ((v #:init-value #f #:accessor fp-v)))
(let ((boxes (vector-map (lambda (_) (make <fp-box>)) (make-vector 200 #f))))
  (do ((round 0 (+ round 1))) ((= round 6))
    (collect-garbage)
    (vector-for-each
      (lambda (b) (set! (fp-v b) (list round (make-vector 3 round))))
      boxes)
    (collect-garbage)
    (test-assert
      (let loop ((i 0))
        (or (= i 200)
            (and (equal? (fp-v (vector-ref boxes i)) (list round (make-vector 3 round)))
                 (loop (+ i 1))))))))

(test-end "boot class accessor fast path")
