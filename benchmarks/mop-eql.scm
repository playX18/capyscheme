;; Eql benchmark.
(import (rnrs) (scheme time) (capy))

(define-generic eql-classify)
(define-method (eql-classify (x <fixnum>)) 'number)
(define-method (eql-classify (x (eql 0))) 'zero)

(define-generic class-classify)
(define-method (class-classify (x <fixnum>)) 'number)
(define-method (class-classify (x <flonum>)) 'flo)

(define calls 30000000)

(define (run-eql x)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (if (eql-classify x) (+ sum 1) sum)))))

(define (run-class x)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (if (class-classify x) (+ sum 1) sum)))))

(define (timed label thunk)
  (thunk)
  (let* ([start (current-jiffy)]
         [result (thunk)]
         [elapsed (/ (exact->inexact (- (current-jiffy) start))
                     (jiffies-per-second))])
    (display (list label 'result result 'seconds elapsed))
    (newline)))

(timed 'eql-match (lambda () (run-eql 0)))
(timed 'eql-nonmatch (lambda () (run-eql 7)))
(timed 'class-only (lambda () (run-class 7)))
