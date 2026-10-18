;; Generic dispatch cost against the number of receiver classes.
;;   capy -L lib -s benchmarks/clos-megamorphic.scm

(import (rnrs) (srfi 1) (scheme time) (capy))

(define-class <base> () ())
(define-generic area)
(define-method (area (o <base>)) 1)

(define (make-objects n)
  (list->vector
   (map (lambda (i)
          (make-instance
           (make-class (string->symbol (string-append "k" (number->string n) "-" (number->string i)))
                       '() (list <base>))))
        (iota n))))

(define calls 2000000)

(define (run objs)
  (let ([n (vector-length objs)])
    (let loop ([i 0] [sum 0])
      (if (= i calls)
          sum
          (loop (+ i 1) (+ sum (area (vector-ref objs (modulo i n)))))))))

(for-each
 (lambda (n)
   (let ([objs (make-objects n)])
     (run objs)
     (let* ([start (current-jiffy)]
            [result (run objs)]
            [elapsed (/ (exact->inexact (- (current-jiffy) start)) (jiffies-per-second))])
       (display (list 'classes n 'result result 'seconds elapsed))
       (newline))))
 '(2 8 64 512 2048))
