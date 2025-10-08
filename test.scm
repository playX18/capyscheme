
(define (fac n)
    (if (= n 0)
        1
        (* n (fac (- n 1)))))
(define x (fac 3000))
(define (make-tree depth)
    (if (= depth 0)
        '()
        (cons (make-tree (- depth 1))
              (make-tree (- depth 1)))))
(define (foo i) (lp i))
(define (lp i)
    
    (if (< i x)
        (begin 
            (make-vector 16)
            (foo (+ i 1)))))
(lp 0)