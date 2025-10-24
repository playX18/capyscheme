(library (core control)
    (export 
        call/cc
        call-with-current-continuation
        dynamic-wind
        when unless case-lambda
        print-condition
        stack-trace
        do
        when
        unless
        case-lambda)
    (import (core primitives)
            (core records))
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

  (define (print-condition exn p)
    (define (print-syntax form subform)
      (define form-src (if (syntax? form) (syntax-sourcev form) #f))
      (define subform-src (if (and subform (syntax? subform)) (syntax-sourcev subform) #f))
      (define (fmt-source src)
        (define file (vector-ref src 0))
        (define line (vector-ref src 1))
        (define col  (vector-ref src 2))
        (format p "~a:~a:~a" file line col))
      (format p "~a" (syntax->datum form))
      (when form-src 
        (format p "~%       in ")
        (fmt-source form-src))
      (when subform
        (format p "~a" (syntax->datum subform))
        (when subform-src 
          (format p "~%     in")
          (fmt-source subform-src))))

    (cond 
      [(condition? exn)
        (let ([c* (simple-conditions exn)])
          (format p "The condition has ~a components:~%" (length c*))
          (do ([i 1 (+ 1 i)]
               [c* c* (cdr c*)])
              [(null? c*)]
            (let* ([c (car c*)]
                   [rtd (record-rtd c)])
              (format p " ~a. " i)
              (let ([supress-type
                      (and (eq? (record-type-parent rtd)
                                (record-type-descriptor &condition))
                           (let ((name (symbol->string (record-type-name rtd)))
                                 (fields (record-type-field-names rtd)))
                             (and (not (eqv? 0 (string-length name)))
                                  (char=? (string-ref name 0) #\&)
                                  (fx>? (vector-length fields) 0)
                                  (string=? (substring name 1 (string-length name))
                                            (symbol->string (vector-ref fields 0))))))])
                  (if supress-type 
                    (put-char p #\&)
                    (let loop ([rtd rtd])
                      (format p "~a" (record-type-name rtd))
                      (cond 
                        [(record-type-parent rtd) => 
                          (lambda (parent)
                            (unless (eq? parent (record-type-descriptor &condition))
                              (format p " ")
                              (loop parent)))])))
                  (let loop ([rtd rtd])
                    (do ([f* (record-type-field-names rtd)]
                         [i 0 (+ i 1)])
                        [(= i (vector-length f*))
                         (cond [(record-type-parent rtd) => loop])]
                      (unless (and supress-type (eqv? i 0))
                        (format p "~%     "))
                      (format p "~a: " (vector-ref f* i))
                      (let ([x ((record-accessor rtd i) c)])
                        (cond 
                          [(and (eq? rtd (record-type-descriptor &syntax)))
                            (print-syntax (syntax-violation-form c) (syntax-violation-subform c))]
                          [(and (eq? rtd (record-type-descriptor &stacktrace)))
                            (display "..." p)]
                          [(and (eq? rtd (record-type-descriptor &irritants))
                                (pair? x)
                                (list? x))
                            (display "(" p)
                            (write (car x) p)
                            (for-each 
                              (lambda (x)
                                (display "\n                 " p)
                                (write x p))
                              (cdr x))
                            (display ")" p)]
                          [else (write x p)]))))))
              (newline p)))]
      [else 
        (format p "A non-condition object was raised:~%~s" exn)]))
  (define (stack-trace k p)
    "Print the stacktrace K to port P. If K is #f, use the current stacktrace."
    (define who 'stack-trace)
    (define (print . x) (for-each (lambda (x) (display x p)) x) (newline p))
    (define stack (if k k (shadow-stack)))

    (format p "Stack trace (most recent call first):~%")
    (let lp ([frame 0] [idx 1] [stk stack])
      (cond 
        [(null? stk) (format p "End of stack trace.~%")]
        [else 
          (let* ([frame-info (car stk)]
                 [stk (cdr stk)]
                 [ip (vector-ref frame-info 0)]
                 [src (vector-ref frame-info 1)]
                 [rator (vector-ref frame-info 2)]
                 [rands  (vector-ref frame-info 3)])
            ;(define proc-name (if (procedure? rator)
              ;(if (interpreted-procedure? rator)
              ;  (assq 'name (interpreted-procedure-meta rator))
              ;  (assq 'name (procedure-properties rator)))
              ;#f))
            (define proc-name (procedure-properties rator))
            (define real-src src)
            
            (format p " at ")
            (if src
              (format p "~a:~a:~a" (vector-ref real-src 0) (vector-ref real-src 1) (vector-ref real-src 2))
              (format p "<unknown file>"))
           
            (newline p)
            (lp frame (+ 1 idx) stk))])))      
        
  )