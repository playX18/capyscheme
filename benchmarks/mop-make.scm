;; Make benchmark.
(import (rnrs) (scheme time) (capy))

(define-class <mk-plain> () ((a #:init-keyword #:a #:init-value 1)))
(define-class <mk-defaults> ()
  ((a #:init-keyword #:a) (b #:init-keyword #:b))
  #:default-initargs ((#:a 10) (#:b 20)))
(define-method (initialize (o <mk-defaults>) initargs)
  (next-method)
  (slot-set! o 'b (+ (slot-ref o 'b) 1)))

(define calls 3000000)

(define (make-loop cls)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (slot-ref (make cls) 'a))))))

(define (class-of-loop o)
  (let loop ([i 0] [n 0])
    (if (= i calls)
        n
        (loop (+ i 1) (if (eq? (class-of o) <mk-plain>) (+ n 1) n)))))

(define (timed label thunk)
  (thunk)
  (let* ([start (current-jiffy)]
         [result (thunk)]
         [elapsed (/ (exact->inexact (- (current-jiffy) start))
                     (jiffies-per-second))])
    (display (list label 'result result 'seconds elapsed))
    (newline)))

(timed 'make-plain (lambda () (make-loop <mk-plain>)))
(timed 'make-defaults (lambda () (make-loop <mk-defaults>)))
(timed 'class-of (lambda () (class-of-loop (make <mk-plain>))))
