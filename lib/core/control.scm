(library (core control)
  (export
    call/cc
    call-with-current-continuation
    call/1cc
    call-in-continuation
    dynamic-wind
    when
    unless
    case-lambda
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
          (datum->syntax stx (length (syntax->datum '(expr ...))))])))
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
          #'(assertion-violation 'case-lambda
             (format #f "unexpected number of arguments, got ~a" n)
             args))
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
    (lambda (x)
      ;; A proper list of identifiers is a fixed-arity formals list. Anything
      ;; else (a dotted list, e.g.) keeps the legacy arity dispatch.
      (define (fixed-formals? f)
        (syntax-case f ()
          (() #t)
          ((a . d) (and (identifier? #'a) (fixed-formals? #'d)))
          (_ #f)))
      (define (all-fixed? clauses)
        (syntax-case clauses ()
          (() #t)
          (((fmls b ...) rest ...)
            (and (fixed-formals? #'fmls) (all-fixed? #'(rest ...))))
          (_ #f)))
      (syntax-case x ()
        ;; One clause is exactly a lambda, variadic or not.
        [(_ (fmls b1 b2 ...))
          #'(lambda fmls b1 b2 ...)]
        ;; Several clauses (fixed, variadic or mixed) lower to one multi-entry
        ;; procedure. `%case-lambda` is a core form in both front ends. The
        ;; multi-entry lowering is on by default (`CAPY_CASE_LAMBDA=1`); set
        ;; the variable to 0 to keep the legacy expansion. Both paths must
        ;; behave identically.
        [(_ (fmls b1 b2 ...) ...)
          (let ((clauses? (pair? (syntax->datum #'((fmls b1 b2 ...) ...))))
                (multi-entry? #t))
            (if (and clauses? multi-entry?)
              #'(|#%case-lambda| (lambda fmls b1 b2 ...) ...)
              ;; A zero-clause case-lambda has no body to lower; the legacy
              ;; expansion raises on every call.
              #'(lambda args
                  (let ((n (length args)))
                    (case-lambda-aux args n (fmls b1 b2 ...) ...)))))])))

  (define %stacktrace-key '|stacktrace-key b8bec3ca-8174-4219-a964-b1aa2aa53ed5|)

  (define (stack-trace k p)
    "Print the stacktrace from marks K to port P. If K is #f, use the current stacktrace."
    (define who 'stack-trace)
    (define (print . x) (for-each (lambda (x) (display x p)) x) (newline p))
    (define (sourcev-ref src index default)
      (if (and (vector? src) (< index (vector-length src)))
        (vector-ref src index)
        default))
    (define (print-source src)
      (let ([file (sourcev-ref src 0 #f)]
            [line (sourcev-ref src 1 #f)]
            [col (sourcev-ref src 2 #f)]
            [end-line (sourcev-ref src 3 #f)]
            [end-col (sourcev-ref src 4 #f)])
        (cond
          [(and file line col end-line end-col)
            (format p "  at ~a:~a:~a-~a:~a:~%" file line col end-line end-col)
            (render-source-line p file line col end-line end-col)]
          [(and file line col)
            (format p "  at ~a:~a:~a:~%" file line col)
            (render-source-line p file line col #f #f)]
          [else
            (format p "  at <unknown file>:~%")])))
    (define marks (if k k (current-continuation-marks)))
    (define stack (continuation-mark-set->list marks %stacktrace-key))

    (if (null? stack)
      (values)
      (begin
        (format p "Stack trace (most recent call first):~%")
        (let lp ([frame 0] [idx 1] [stk stack])
          (cond
            [(null? stk) (format p "End of stack trace.~%")]
            [else
              (let* ([fp (car stk)]
                     [src (vector-ref fp 0)]
                     [proc (vector-ref fp 1)]
                     [args (vector-ref fp 2)])

                (print-source src)

                (cond
                  [(interpreted-procedure? proc)
                    =>
                    (lambda (meta)
                      (format p "(interpreted ~a)" meta))]
                  [(interpreted-expression? proc)
                    =>
                    (lambda (meta)
                      (format p "(interpreted expression ~a)" meta))])

                (cond
                  [(procedure-name proc)
                    =>
                    (lambda (name)
                      (format p "\t~s~%" (cons name (syntax->datum args))))]
                  [else
                    (format p "\t~s~%" (cons '<anonymous> (syntax->datum args)))])

                (lp (+ frame 1) (+ idx 1) (cdr stk)))]))))))
