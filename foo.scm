
(define stacktrace-key '|stacktrace-key b8bec3ca-8174-4219-a964-b1aa2aa53ed5|)

(define (print-stacktrace)
    (define trace (continuation-mark-set->list (current-continuation-marks) stacktrace-key))

    (for-each 
        (lambda (mark)
            (define src (vector-ref mark 0))
            (define proc (procedure-name (vector-ref mark 1)))
            (define args (vector-ref mark 2))
            
            (cond 
                [src 
                    (format #t "  at ~a:~a:~a: ~a~a~%" (vector-ref src 0) (vector-ref src 1) (vector-ref src 2) proc args)]
                [else 
                    (format #t "  at <unknown>: ~a~a~%" proc args)]))


        trace))

(define (fac n acc)
  (if (= n 0)
      (begin 
      
        (print-stacktrace)
        acc)
      (fac (- n 1) (* n acc))))

(format #t "Factorial of 5 is ~a~%" (fac 500 1))