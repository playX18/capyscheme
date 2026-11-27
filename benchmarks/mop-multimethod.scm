;; Multimethod benchmark.
(import (rnrs) (scheme time) (capy))

(define-class <mm-a> () ())
(define-class <mm-b> (<mm-a>) ())
(define-generic mm-pair)
(define-method (mm-pair (x <mm-a>) (y <mm-a>)) 1)
(define-method (mm-pair (x <mm-b>) (y <mm-a>)) 2)
(define-method (mm-pair (x <mm-a>) (y <mm-b>)) 3)

(define a (make <mm-a>))
(define b (make <mm-b>))
(define calls 30000000)

(define (pair-loop x y)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (mm-pair x y))))))

(define (timed label thunk)
  (thunk)
  (let* ([start (current-jiffy)]
         [result (thunk)]
         [elapsed (/ (exact->inexact (- (current-jiffy) start))
                     (jiffies-per-second))])
    (display (list label 'result result 'seconds elapsed))
    (newline)))

(timed 'multimethod-aa (lambda () (pair-loop a a)))
(timed 'multimethod-ba (lambda () (pair-loop b a)))
(timed 'multimethod-ab (lambda () (pair-loop a b)))
