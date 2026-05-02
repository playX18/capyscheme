(import (core)
        (core control)
        (srfi 64))

(test-begin "boot control")

(define (captured-mark-result)
  (define result
    (with-continuation-mark 'base 'captured
      (call/cc (lambda (k) k))))
  (if (continuation? result)
    (call-in-continuation
      result
      (lambda ()
        (continuation-mark-set-first
          (current-continuation-marks)
          'base
          'missing)))
    result))

(define (explicit-mark-set-result)
  (define result
    (with-continuation-mark 'base 'captured
      (call/cc (lambda (k) k))))
  (if (continuation? result)
    (call-in-continuation
      result
      (continuation-next-marks result)
      (lambda ()
        (continuation-mark-set-first
          (current-continuation-marks)
          'base
          'missing)))
    result))

(define (dynamic-wind-result)
  (define events '())
  (define result
    (dynamic-wind
      (lambda () (set! events (cons 'in events)))
      (lambda () (call/cc (lambda (k) k)))
      (lambda () (set! events (cons 'out events)))))
  (if (continuation? result)
    (let ([events-after-capture events])
      (call-in-continuation
        result
        (lambda ()
          (set! events (cons 'proc events))
          (list 'returned events-after-capture events))))
    (list result events)))

(test-equal "continuation-predicate"
  (continuation? (call/cc (lambda (k) k)))
  #t)

(test-equal "call-in-continuation-values"
  (call-with-values
    (lambda ()
      (call/cc
        (lambda (k)
          (call-in-continuation
            k
            (lambda () (values 'a 'b))))))
    list)
  '(a b))

(test-equal "call-in-continuation-restores-marks"
  (captured-mark-result)
  'captured)

(test-equal "call-in-continuation-explicit-mark-set"
  (explicit-mark-set-result)
  'captured)

(test-equal "call-in-continuation-runs-winders"
  (dynamic-wind-result)
  '((returned (out in) (proc out in)) (out in proc out in)))

(test-end "boot control")
