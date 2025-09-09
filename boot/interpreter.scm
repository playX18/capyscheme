(define (interpret/preprocess expr env)
    (cond 
        [(lref? expr) 
            (let* ([address (interpret/var-address (lref-variable expr) env)]
                   [rib (car address)]
                   [offset (cdr address)])
                (lambda (env0)
                    (let loop ([rib rib] [env env0])
                        (if (zero? rib)
                            (vector-ref (car env) offset)
                            (loop (- rib 1) (cdr env))))))]
        [(constant? expr)
            (lambda (env) (constant-value expr))]
        [(application? expr)
            (let* ([rator (interpret/preprocess (application-operator expr) env)]
                   [rands (map (lambda (rand) (interpret/preprocess rand env)) (application-operands expr))])
                (lambda (env0)
                    (let loop ([rands rands] [vals '()])
                        (if (null? rands)
                            (apply (rator env0) (reverse vals))
                            (loop (cdr rands) (cons ((car rands) env0) vals))))))]
        [toplevel-ref? expr
            (let ([name (toplevel-ref-name expr)]
                  [var #f])
                (lambda (env)
                    (if var 
                        (variable-ref var)
                        (begin 
                            (set! var (module-variable (current-module) name))
                            (variable-ref var)))))]
        [(toplevel-set? expr)
            (let ([name (toplevel-set-name expr)]
                  [val-proc (interpret/preprocess (toplevel-set-value expr) env)]
                  [var #f])
                (lambda (env0)
                    (if var 
                        (variable-set! var (val-proc env0))
                        (begin 
                            (set! var (module-variable (current-module) name))
                            (variable-set! var (val-proc env0))))))]

        [(if? expr)
            (let* ([test-proc (interpret/preprocess (if-test expr) env)]
                   [then-proc (interpret/preprocess (if-then expr) env)]
                   [else-proc (interpret/preprocess (if-else expr) env)])
                (lambda (env0)
                    (if (test-proc env0)
                        (then-proc env0)
                        (else-proc env0))))]

        [(let*? expr)
            (let ([lhs (let-lhs expr)]
                    [rhs (let-rhs expr)]
                    [body (let-body expr)])
                (let loop ([vars lhs] [vals rhs] [env env])
                    (if (null? vars)
                        (interpret/preprocess body env)
                        (let* ([val-proc (interpret/preprocess (car vals) env)]
                                [nenv (cons (list (car vars)) env)])
                            (let ([rest-proc (loop (cdr vars) (cdr vals) nenv)])
                                (lambda (env0)
                                    (let ([val (val-proc env0)])
                                        (rest-proc (cons (vector val) env0)))))))))]
        [(letrec? expr)
            (let* ([lhs (let-lhs expr)]
                    [rhs (let-rhs expr)]
                    [body (let-body expr)]
                    [nenv (cons lhs env)]
                    [rhs-procs (map (lambda (rhs) (interpret/preprocess rhs nenv)) rhs)]
                    [body-proc (interpret/preprocess body nenv)])
                (lambda (env0)
                    (let* ([rib (make-vector (length lhs) #f)]
                            [new-env (cons rib env0)])                        
                        (let loop ([i 0] [procs rhs-procs])
                            (if (null? procs)
                                (body-proc new-env)
                                (begin
                                    (vector-set! rib i ((car procs) new-env))
                                    (loop (+ i 1) (cdr procs))))))))]
        [(let? expr)
            (let* ([lhs (let-lhs expr)]
                   [rhs (map (lambda (rhs) (interpret/preprocess rhs env)) (let-rhs expr))]
                   [body (let-body expr)]
                   [nenv (cons lhs env)])
                (let ([body-proc (interpret/preprocess body nenv)])
                    (lambda (env0)
                        (let loop ([rhs rhs] [vals '()])
                            (if (null? rhs)
                                (body-proc (cons (list->vector (reverse vals)) env0))
                                (loop (cdr rhs) (cons ((car rhs) env0) vals)))))))]
        
        [else (lambda (env0) #f)]))



(define (interpret/var-address name env)
    (let r-loop ([env env] [i 0])
        (if (null? env)
            #f 
            (let a-loop ([rib (car env)] [j 0])
                (cond 
                    [(null? rib) (r-loop (cdr env) (+ i 1))]
                    [(eq? (car rib) name) (cons i j)]
                    [else (a-loop (cdr rib) (+ j 1))])))))


(define check 
    (make-application #f 
        (make-toplevel-ref #f 'mod '+)
        (list (make-constant #f 1)
              (make-constant #f 2)
              (make-constant #f 3))))
(define clos (interpret/preprocess check '()))

(print "interp=" (clos '()))