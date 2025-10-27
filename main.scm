  (define list-sort
    (lambda (proc lst)

      (define merge
        (lambda (lst1 lst2)
          (format #t "merge lst1=~a lst2=~a~%" lst1 lst2)
          (cond
           ((null? lst1) lst2)
           ((null? lst2) lst1)
           (else
            (if (proc (car lst2) (car lst1))
                (cons (car lst2) (merge lst1 (cdr lst2)))
                (cons (car lst1) (merge (cdr lst1) lst2)))))))

      (define sort
        (lambda (lst n)
          (cond ((= n 1)
                 (list (car lst)))
                ((= n 2)
                 (if (proc (cadr lst) (car lst))
                     (list (cadr lst) (car lst))
                     (list (car lst) (cadr lst))))
                (else
                 (let ((n/2 (div n 2)))
                   (merge (sort lst n/2)
                          (sort (list-tail lst n/2) (- n n/2))))))))

      (define divide
        (lambda (lst)
          (let loop ((acc 1) (lst lst))
            (cond ((null? (cdr lst)) (values acc '()))
                  (else
                   (format #t "divide car=~a cdr=~a~%" (car lst) (cadr lst))
                   (if (proc (car lst) (cadr lst))
                       (loop (+ acc 1) (cdr lst))
                       (values acc (cdr lst))))))))

      (cond ((null? lst) '())
            (else
             (let ((len (length lst)))
               (let-values (((n rest) (divide lst)))
                 (format #t "n=~a rest=~a, head=~a~%" n rest (list-head lst n))
                 (cond ((null? rest) lst)
                       (else
                        (merge (list-head lst n)
                               (sort rest (- len n)))))))))))

(format #t "~a~%" (list-sort < '(3 2 1)))