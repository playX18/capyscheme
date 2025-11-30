(define (main)
  (define (double x) (* x 2))
  (define f1 double)

  (printf ("~a~%" (f1 10)))
  (printf ("~a~%" (double 5)))
  )