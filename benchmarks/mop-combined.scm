;; Combined benchmark.
(import (rnrs) (scheme time) (capy))

(define-class <cb-a> () ())
(define-generic cb-run)
(define-method (cb-run (o <cb-a>) n) (+ n 1))
(define-method (cb-run (o <cb-a>) n) #:before (set! cb-count (+ cb-count 1)))
(define-method (cb-run (o <cb-a>) n) #:after (set! cb-count (+ cb-count 1)))

(define cb-count 0)
(define cb-obj (make <cb-a>))
(define calls 10000000)

(define (combined-loop o)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (cb-run o i))))))

(define (hand-written-run n)
  (set! cb-count (+ cb-count 1))
  (let ([v (+ n 1)])
    (set! cb-count (+ cb-count 1))
    v))

(define (hand-loop)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (hand-written-run i))))))

(define (timed label thunk)
  (thunk)
  (let* ([start (current-jiffy)]
         [result (thunk)]
         [elapsed (/ (exact->inexact (- (current-jiffy) start))
                     (jiffies-per-second))])
    (display (list label 'result result 'seconds elapsed))
    (newline)))

(timed 'before-after-generic (lambda () (combined-loop cb-obj)))
(timed 'hand-written (lambda () (hand-loop)))
