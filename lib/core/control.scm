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
            (capy)
            (core records))

  (define (reverse! ls)
    (let loop ([l ls] [a '()])
      (cond 
        [(pair? l)  
          (let ([x (cdr l)])
            (set-cdr! l a)
            (loop x l))]
        [(null? l) a]
        [else 
          (let loop ([l a] [a l])
            (let ([x (cdr l)])
              (set-cdr! l a)
              (loop x l)))])))

  (define-syntax cte-length 
    (lambda (stx)
      (syntax-case stx () 
        [(_ (expr ...))
         (datum->syntax stx (length (syntax->datum '(expr ...))))]
      )
    ))
    (define-syntax case-lambda-aux
    (lambda (x)
      (define (construct args formals clause*)
        (define _car #'car)
        (define _cdr #'cdr)
        (define (parse-formals formal args inits)
          (syntax-case formal ()
            (() (reverse! inits))
            ((a . d)
              (with-syntax ((arg `(,_car ,args))
                (args `(,_cdr ,args)))
                (parse-formals #'d #'args
                    (cons (list #'a #'arg) inits))))
            (v
              (reverse! (cons (list #'v args) inits)))))
        (with-syntax 
               ((((var init) ...) (parse-formals formals args '()))
                ((clause ...) clause*))
          ;; Using `lambda` enables type check, immediate apply
          ;; will be converted to let by the compiler.
          #'((lambda (var ...) clause ...) init ...)))
      (syntax-case x ()
	((_ args n)
	 #'(assertion-violation #f "unexpected number of arguments" args))
	((_ args n ((x ...) b ...) more ...)
	 (with-syntax ((let-clause (construct #'args #'(x ...) #'(b ...)))
		       (expect-length (length #'(x ...))))
	   #'(if (= n expect-length)
		 let-clause
		 (case-lambda-aux args n more ...))))
	((_ args n ((x1 x2 ... . r) b ...) more ...)
	 (with-syntax ((let-clause (construct #'args #'(x1 x2 ... . r) 
					      #'(b ...)))
		       (expect-length (length #'(x1 x2 ...))))
	   #'(if (>= n expect-length)
		 let-clause
		 (case-lambda-aux args n more ...))))
	((_ args n (r b ...) more ...)
	 #'(let ((r args)) b ...)))))

  (define-syntax case-lambda
    (syntax-rules ()
      ((_ (fmls b1 b2 ...))
       (lambda fmls b1 b2 ...))
      ((_ (fmls b1 b2 ...) ...)
       (lambda args
	 (let ((n (length args)))
	   (case-lambda-aux args n (fmls b1 b2 ...) ...))))))


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
              [(interpreted-procedure? proc)
                => (lambda (meta)
                  (format p "(interpreted ~a)" meta))]
              [(interpreted-expression? proc) 
                => (lambda (meta)
                  (format p "(interpreted expression ~a)" meta))])

            (cond 
              [(procedure-name proc)
                => (lambda (name)
                      (format p "\t~s~%" (cons name (syntax->datum args))))]
              [else 
                (format p "\t~s~%" (cons '<anonymous> (syntax->datum args)))])
            
            (lp (+ frame 1) (+ idx 1) (cdr stk)))]))))