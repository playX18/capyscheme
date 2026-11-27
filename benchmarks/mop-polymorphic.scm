;; Polymorphic benchmark.
(import (rnrs) (scheme time) (capy))

(define-class <poly-a> () ())
(define-class <poly-b> () ())
(define-class <poly-c> () ())
(define-class <poly-d> () ())
(define-generic poly-area)
(define-method (poly-area (o <poly-a>)) 1)
(define-method (poly-area (o <poly-b>)) 2)
(define-method (poly-area (o <poly-c>)) 3)
(define-method (poly-area (o <poly-d>)) 4)

(define objs (vector (make <poly-a>) (make <poly-b>) (make <poly-c>) (make <poly-d>)))
(define calls 30000000)

(define (poly-loop)
  (let ([n (vector-length objs)])
    (let loop ([i 0] [sum 0])
      (if (= i calls)
          sum
          (loop (+ i 1) (+ sum (poly-area (vector-ref objs (modulo i n)))))))))

(define (timed label thunk)
  (thunk)
  (let* ([start (current-jiffy)]
         [result (thunk)]
         [elapsed (/ (exact->inexact (- (current-jiffy) start))
                     (jiffies-per-second))])
    (display (list label 'result result 'seconds elapsed))
    (newline)))

(timed 'polymorphic-4 (lambda () (poly-loop)))
