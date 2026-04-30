(import (core threading) (core arithmetic) (scheme process-context))

(define (item-check tree)
  (define left (car tree))
  (define right (cdr tree))
  (if (null? left)
    1
    (+ 1 (item-check left) (item-check right))))

(define (bottom-up-tree depth)
  (if (> depth 0)
    (cons (bottom-up-tree (- depth 1)) (bottom-up-tree (- depth 1)))
    (cons '() '())))

(define n (string->number (cadr (command-line))))
(define min-depth 4)
(define max-depth (if (< n (+ 2 min-depth)) (+ 2 min-depth) n))
(define stretch-depth (+ max-depth 1))
(define safe-printf
  (let ([mtx (make-mutex)])
    (lambda (fmt . args)
      (with-mutex mtx
        (apply printf fmt args)))))

(define start (microsecond))

(printf "stretch tree of depth ~a\t check: ~a~%" stretch-depth (item-check (bottom-up-tree stretch-depth)))

(let
  ([long-lived-tree (bottom-up-tree max-depth)]
    [results (make-vector (+ 1 (quotient (- max-depth min-depth) 2)))])

  (do ([d min-depth (+ d 2)])
    ((> d max-depth) #t)
    (let ([depth d])

      (define iterations (bitwise-arithmetic-shift 1 (+ (- max-depth depth) min-depth)))
      (let loop ([check 0] [i 0])
        (cond
          [(>= i iterations)
            (vector-set! results
              (quotient (- max-depth depth) 2)
              (format #f "~a\t trees of depth ~a\t check: ~a" iterations depth check))]
          [else
            (loop (+ check (item-check (bottom-up-tree depth))) (+ i 1))]))))

  (for-each (lambda (result) (printf "~a~%" result)) (vector->list results))
  (printf "long lived tree of depth ~a\t check: ~a~%" max-depth (item-check long-lived-tree)))

(define end (microsecond))

(let ([elapsed (- end start)]
      [two-sec-us 2000000])
  (if (< elapsed two-sec-us)
    (printf ";; elapsed ~a ms~%" (/ elapsed 1000.0))
    (printf ";; elapsed ~a s~%" (/ elapsed 1000000.0))))
