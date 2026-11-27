;; Accessor benchmark.
(import (rnrs) (scheme time) (capy))

(define-class <acc-point> () ((x #:init-value 3 #:accessor acc-x)))
(define p (make <acc-point>))
(define calls 30000000)

(define (accessor-loop o)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (acc-x o))))))

(define (slot-ref-loop o)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (slot-ref o 'x))))))

(define (timed label thunk)
  (thunk)
  (let* ([start (current-jiffy)]
         [result (thunk)]
         [elapsed (/ (exact->inexact (- (current-jiffy) start))
                     (jiffies-per-second))])
    (display (list label 'result result 'seconds elapsed))
    (newline)))

(timed 'accessor (lambda () (accessor-loop p)))
(timed 'slot-ref (lambda () (slot-ref-loop p)))
