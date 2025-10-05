(define (interpret/preprocess expr env)
    (cond
        [(lref? expr)
            (let* ([address (interpret/var-address (lref-sym expr) env)]
                   [rib (car address)]
                   [offset (cdr address)])
                (lambda (env0)
                    (let loop ([rib rib] [env env0])
                        (if (zero? rib)
                            (let ([val (vector-ref (car env) offset)])
                                val)
                            (loop (- rib 1) (cdr env))))))]
        [(lset? expr)
            (let* ([address (interpret/var-address (lset-sym expr) env)]
                   [rib (car address)]
                   [offset (cdr address)]
                   [val-proc (interpret/preprocess (lset-value expr) env)])
                (lambda (env0)
                    (let loop ([rib rib] [env env0])
                        (if (zero? rib)
                            (vector-set! (car env) offset (val-proc env0))
                            (loop (- rib 1) (cdr env))))))]
        [(constant? expr)
            (lambda (env) (constant-value expr))]
        [(application? expr)
            (let* ([rator (interpret/preprocess (application-operator expr) env)]
                   [rands (map (lambda (rand) (interpret/preprocess rand env)) (application-operands expr))])
                (lambda (env0)
                    (let ([func (rator env0)])
                        (apply func (map (lambda (rand) (rand env0)) rands)))))]
        [(primref? expr)
            (let* ([name (primref-prim expr)]
                   [func (lookup-bound '(capy) name #t)])
                (lambda (env) (variable-ref func)))]
        [(primcall? expr)
            (let* ([rator (primcall-prim expr)]
                   [rands (map (lambda (rand) (interpret/preprocess rand env)) (primcall-args expr))]
                   [func (lookup-bound '(capy) rator #t)])
                (lambda (env0)
                    (let loop ([rands rands] [vals '()])
                        (if (null? rands)
                            (apply (variable-ref func) (reverse vals))
                            (loop (cdr rands) (cons ((car rands) env0) vals))))))]
        [(proc? expr) (interpret/lambda expr env)]
        [(toplevel-define? expr)
            (let ([name (toplevel-define-name expr)]
                  [val (interpret/preprocess (toplevel-define-value expr) env)]
                  [module-slot #f])
                (lambda (env0)
                    (if module-slot
                        (module-define! module-slot name (val env0))
                        (begin
                            (set! module-slot (current-module))
                            (module-define! module-slot name (val env0))))))]
        [(toplevel-ref? expr)
            (let ([name (toplevel-ref-name expr)]
                  [var #f])
                (lambda (env)
                    (if var
                        (variable-ref var)
                        (begin
                            (set! var (module-variable (current-module) name))
                            (if (not var)
                                (error 'toplevel-ref "unbound variable" name))
                            (variable-ref var)))))]
        [(module-ref? expr)
            (let ([module (module-ref-module expr)]
                  [name (module-ref-name expr)]
                  [public? (module-ref-public? expr)]
                  [var #f])
                (lambda (env0)
                    (if var 
                        (variabe-ref var)
                        (begin 
                            (set! var (lookup-bound module name public?))
                            (variable-ref var)))))]
        [(module-set? expr)
            (let ([module (module-set-module expr)]
                  [name (module-set-name expr)]
                  [public? (module-set-public? expr)]
                  [val-proc (interpret/preprocess (module-set-value expr) env)]
                  [var #f])
                (lambda (env0)
                    (if var
                        (variable-set! var (val-proc env0))
                        (begin
                            (set! var (lookup-bound module name public?))
                            (variable-set! var (val-proc env0))))))]
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
        [(or (letrec*? expr) (letrec? expr))
            (let* ([lhs (let-lhs expr)]
                    [rhs (let-rhs expr)]
                    [body (let-body expr)]
                    [nenv (cons lhs env)]
                    [rhs-procs (map (lambda (rhs) (interpret/preprocess rhs nenv)) rhs)]
                    [body-proc (interpret/preprocess body nenv)])
                (lambda (env0)
                    (let* ([rib (make-vector (length lhs) 555)]
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
        [(sequence? expr)
            (let* ([head (interpret/preprocess (sequence-head expr) env)]
                   [tail (interpret/preprocess (sequence-tail expr) env)])
                (lambda (env0)
                    (head env0)
                    (tail env0)))]
        [(void? expr)
            (lambda (env0) (if #f #f))]
        [else (lambda (env0) (assertion-violation #f "unhandled node " expr))]))



(define (interpret/lambda expr env)
    (define (listify x)
        (cond ((pair? x) (cons (car x) (listify (cdr x))))
            ((null? x) x)
            (else (list x))))

    (define (fixed-args x n)
        (if (pair? x)
            (fixed-args (cdr x) (+ n 1))
            n))
(let*   ((args  (proc-args expr))
         (body  (proc-body expr))
         (nenv  (interpret/extend-env env (cons '&self (listify args))))
         (exprs (interpret/preprocess body nenv)))
    (cond ((pair? args)
           (let ((tail0 (cdr args)))
             (cond ((pair? tail0)
                    (let ((tail1 (cdr tail0)))
                      (cond ((pair? tail1)
                             (let ((tail2 (cdr tail1)))
                               (cond ((pair? tail2)
                                      (let ((tail3 (cdr tail2)))
                                        (cond ((pair? tail3)
                                               (if (list? tail3)
                                                   (interpret/lambda-n (length args) exprs )
                                                   (interpret/lambda-dot (fixed-args args 0) exprs)))
                                              ((null? tail3) (interpret/lambda4 exprs))
                                              (else (interpret/lambda-dot (fixed-args args 0) exprs)))))
                                     ((null? tail2) (interpret/lambda3 exprs))
                                     (else (interpret/lambda-dot (fixed-args args 0) exprs)))))
                            ((null? tail1) (interpret/lambda2 exprs))
                            (else (interpret/lambda-dot (fixed-args args 0) exprs)))))
                   ((null? tail0) (interpret/lambda1 exprs))
                   (else (interpret/lambda-dot (fixed-args args 0) exprs)))))
          ((null? args) (interpret/lambda0 exprs))
          (else (interpret/lambda-dot (fixed-args args 0) exprs)))))

(define (interpret/lambda-n n body)
    (lambda (env)
        (letrec ([self
            (lambda args
                (if (< (length args) n)
                    (assertion-violation 'lambda "wrong number of arguments" (length args))
                    (body (cons (list->vector (cons self args)) env))))])
            self)))

(define (interpret/lambda-dot n body)
    (lambda (env)
        (letrec ([self
            (lambda args
                (let ([v (make-vector (+ n 2) 444)]
                      [limit (+ n 1)])
                    (vectoer-set! v 0 self)
                    (let loop ([argnum 1]
                               [argtail args])
                        (cond
                            [(= argnum limit)
                                (vector-set! v argnum argtail)
                                (body (cons v env))]
                            [(pair? argtail)
                                (vector-set! v argnum (car argtail))
                                (loop (+ argnum 1) (cdr argtail))]
                            [else (assertion-violation 'lambda "wrong number of arguments" (length args))]))))])
            self)))

(define (interpret/lambda0 body)
    (lambda (env)
        (letrec ([self
            (lambda ()
                (body (cons (vector self) env)))])
            self)))
(define (interpret/lambda1 body)
    (lambda (env)
        (letrec ([self
            (lambda (arg1)
                (let ([env (cons (vector self arg1) env)])
                 
                (body env)))])
            self)))
(define (interpret/lambda2 body)
    (lambda (env)
        (letrec ([self
            (lambda (arg1 arg2)
                (body (cons (vector self arg1 arg2) env)))])
            self)))

(define (interpret/lambda3 body)
    (lambda (env)
        (letrec ([self
            (lambda (arg1 arg2 arg3)
                (body (cons (vector self arg1 arg2 arg3) env)))])
            self)))

(define (interpret/lambda4 body)
    (lambda (env)
        (letrec ([self
            (lambda (arg1 arg2 arg3 arg4)
                (body (cons (vector self arg1 arg2 arg3 arg4) env)))])
            self)))

(define (interpret/var-address name env)
    (let r-loop ([env env] [i 0])
        (if (null? env)
            555
            (let a-loop ([rib (car env)] [j 0])
                (cond
                    [(null? rib) (r-loop (cdr env) (+ i 1))]
                    [(eq? (car rib) name) (cons i j)]
                    [else (a-loop (cdr rib) (+ j 1))])))))

(define (interpret/extend-env env names)
  (cons names env))

(define (primitive-eval exp)
    ((interpret/preprocess exp '()) '()))
