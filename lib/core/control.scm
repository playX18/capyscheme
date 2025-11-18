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
        case-lambda
        with-continuation-mark
        current-continuation-marks
        continuation-marks?
        continuation-mark-set->list
        continuation-mark-set->list*
        continuation-mark-set-first 
        continuation-next-marks
        continuation?)
    (import (core primitives)
            (only (capy) print-condition)
            (core records))

  (define-syntax case-lambda
    (syntax-rules ()
      ((case-lambda (params body0 ...) ...)
      (lambda args
        (let ((len (length args)))
          (letrec-syntax
              ((cl (syntax-rules ::: ()
                      ((cl)
                      (error "no matching clause"))
                      ((cl ((p :::) . body) . rest)
                      (if (= len (length '(p :::)))
                          (apply (lambda (p :::)
                                    . body)
                                  args)
                          (cl . rest)))
                      ((cl ((p ::: . tail) . body)
                          . rest)
                      (if (>= len (length '(p :::)))
                          (apply
                            (lambda (p ::: . tail)
                              . body)
                            args)
                          (cl . rest))))))
            (cl (params body0 ...) ...)))))))


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