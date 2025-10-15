(library (core control)
    (export 
        call/cc
        call-with-current-continuation
        dynamic-wind
        when unless case-lambda)
    (import)
  (define-syntax case-lambda
    (lambda (x)
      (define compile-clause
        (lambda (clause)
          (syntax-case clause ()
            (((x ...) e1 e2 ...)
             (let ((argc (length #'(x ...))))
               (with-syntax ((n argc))
                 (case argc
                   ((0) #'((null? args) e1 e2 ...))
                   ((1) #'((= len n) ((lambda (x ...) e1 e2 ...) (car args))))
                   ((2) #'((= len n) ((lambda (x ...) e1 e2 ...) (car args) (cadr args))))
                   ((3) #'((= len n) ((lambda (x ...) e1 e2 ...) (car args) (cadr args) (caddr args))))
                   (else #'((= len n) (apply (lambda (x ...) e1 e2 ...) args)))))))
            (((x1 x2 ... . r) e1 e2 ...)
             (with-syntax ((n (length #'(x1 x2 ...))))
               #'((>= len n) (apply (lambda (x1 x2 ... . r) e1 e2 ...) args))))
            ((r e1 e2 ...)
             #'(#t (apply (lambda r e1 e2 ...) args))))))
      (syntax-case x ()
        ((_ (e0 e1 e2 ...) ...)
         (with-syntax (((clauses ...) (map compile-clause #'((e0 e1 e2 ...) ...))))
           #'(lambda args
               (let ((len (length args)))
                 (cond
                   clauses ...
                   (else
                    (assertion-violation #f "wrong number of arguments" args))))))))))

)