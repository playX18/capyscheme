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
        continuation?
        $stacktrace-key)
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


  (define %stacktrace-key '|stacktrace-key b8bec3ca-8174-4219-a964-b1aa2aa53ed5|)


  (define (stack-trace k p)
    "Print the stacktrace from marks K to port P. If K is #f, use the current stacktrace."
    (define who 'stack-trace)
    (define (print . x) (for-each (lambda (x) (display x p)) x) (newline p))
    (define marks (if k k (current-continuation-marks)))
    (define stack (continuation-mark-set->list marks %stacktrace-key))

    (format p "Stack trace (most recent call first):~%")
    (let lp ([frame 0] [idx 1] [stk stack])
      (cond 
        [(null? stk) (format p "End of stack trace.~%")]
        [else 
          (let* ([fp (car stk)]
                 [src (vector-ref fp 0)]
                 [proc (vector-ref fp 1)]
                 [args (vector-ref fp 2)])
            
            (cond 
              [src 
                (format p "  at ~a:~a:~a:~%" (vector-ref src 0) (vector-ref src 1) (vector-ref src 2))]
              [else 
                (format p "  at <unknown file>:~%")])

            (cond 
              [(procedure-name proc)
                => (lambda (name)
                      (format p "\t~s~%" (cons name args)))]
              [else 
                (format p "\t~s~%" (cons '<anonymous> args))])
            
            (lp (+ frame 1) (+ idx 1) (cdr stk)))]))))