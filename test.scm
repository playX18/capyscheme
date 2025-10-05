(define (make-tree depth)
    (if (= depth 0)
        '()
        (cons (make-tree (- depth 1)) (make-tree (- depth 1)))))

(define iterations 246000)
(define depth 18)

(let lp ([i 0] [res '()])
    (if (< i iterations)
        (lp (+ i 1) (make-tree depth))
        #f))