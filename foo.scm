#!nobacktrace

(import (rnrs io ports))

(define (bottom-up-tree depth)
    (if (zero? depth)
        '()
        (let ([depth (- depth 1)])
          (cons (bottom-up-tree depth)
                (bottom-up-tree depth)))))

(define (check-tree tree)
    (if (null? tree)
        0
        (+ 1 (check-tree (car tree))
             (check-tree (cdr tree)))))

(define (inner-single-thread depth iterations)
  (let loop ((i 0) (chk 0))
    (if (< i iterations)
        (loop (+ i 1)
              (+ chk (check-tree (bottom-up-tree depth))))
        chk)))

(define n (read (open-string-input-port (cadr (program-arguments)))))

(define min-depth 4)
(define max-depth (if (> (+ min-depth 2) n) (+ min-depth 2) n))

(let ([tree (bottom-up-tree (+ max-depth 1))])
  (format #t "stretch tree of depth ~a\t check: ~a~%" (+ max-depth 1) (check-tree tree)))

(define long-lived-tree (bottom-up-tree max-depth))

(let loop ((depth min-depth))
    (when (<= depth max-depth)
        (let* ((iterations (expt 2 (- max-depth depth)))
               (chk (inner-single-thread depth iterations)))
        (format #t "~a\t trees of depth ~a\t check: ~a~%" iterations depth chk))
        (loop (+ depth 2))))
