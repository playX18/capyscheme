#!nobacktrace

(library (main)
  (export)
  (import (rnrs programs) (rnrs) (capy))

(define (main)
(define (make-tree n)
  (if (= n 0)
    '()
    (cons (make-tree (- n 1))
          (make-tree (- n 1)))))

(define (check-tree tree)
  (cond 
    [(null? tree) 0]
    [else (+ 1 (check-tree (car tree))
               (check-tree (cdr tree)))]))
 
(define min-depth 4)
(define max-depth (max (+ 2 min-depth) (string->number (list-ref (command-line) 1))))
(printf "min depth: ~a~%" min-depth)
(printf "max depth: ~a~%" max-depth) 
(let ([stretch-depth (+ max-depth 1)])
  (printf "stretch tree of depth ~a\t check: ~a~%"
          stretch-depth
          (check-tree (make-tree stretch-depth))))

(define long-lived-tree (make-tree max-depth))

(define iterations (expt 2 max-depth))


(let loop ([depth min-depth])
  (when (<= depth max-depth)
    (let ([check
      (let loop2 ([i 1] [check 0])
        (if (<= i iterations)
          (loop2 (+ i 1) (+ check (check-tree (make-tree depth))))
          check))])
      (printf "~a\t trees of depth ~a\t check: ~a~%"
              (/ iterations 2)
              depth
              check))
    (loop (+ depth 2))))

(printf "long lived tree of depth ~a\t check: ~a~%"
        max-depth
        (check-tree long-lived-tree)))
  (main)      
  )