(define (make-lambda-expander s formals bodys ctx)
    (define sc (new-scope))
    (define phase (expand-context-phase ctx))
    ;; Parse and check formal arguments
    (define ids (parse-and-flatten-formals formals sc))
    ;; Bind each argument and generate corresponding key for 
    ;; expand-time environment
    (define keys (map (lambda (id)
   
     (add-local-binding! id phase)) ids))
    (define body-env (foldl (lambda (env key)
     
        (env-extend env key variable))
        (expand-context-env ctx)
        keys))
    
    (define body-ctx (expand-context-with-new-scope ctx sc body-env))
    (define exp-body (expand-body bodys sc s body-ctx))
    ;; return formals with new scope and expanded body
    (values (add-scope formals sc)
            exp-body))
            
(add-core-form! 
    'lambda
    (lambda (s ctx)
        (define m (match-syntax s '(lambda formals body :::+)))
        (call-with-values 
            (lambda ()
                (make-lambda-expander s (m 'formals) (m 'body) ctx))
            (lambda (formals body)
                (rebuild 
                    s 
                    `(,(m 'lambda) ,formals ,body))))))
                    ;(cons (m 'lambda)
                    ;    (cons formals body)))))))

(add-core-form! 
    'define
    (lambda (s ctx)
        (error 'define "not allowed in expression" s)))

(add-core-form! 
    (string->symbol "#%datum")
    (lambda (s ctx)
        (define m (match-syntax s '(_ . datum)))
        (define phase (expand-context-phase ctx))
        (rebuild
            s
            (list (datum->syntax (syntax-shift-phase-level core-stx phase) 'quote)
                  (m 'datum)))))

(define app-sym (string->symbol "#%app"))

(add-core-form!
 app-sym
 (lambda (s ctx)
   (define m (match-syntax s `(,app-sym rator rand :::)))
   (printf "in #%app~%")
   (rebuild
    s
    (cons* (m app-sym)
           (expand (m 'rator) ctx)
           (map (lambda (rand) (expand rand ctx))
                (m 'rand))))))

(printf "added #%app~%")

(define (parse-and-flatten-formals all-formals sc)
    (let loop ([formals all-formals])
        (cond 
            [(identifier? formals) (list (add-scope formals sc))]
            [(syntax? formals)
                (define p (syntax-e formals))
                (cond 
                    [(pair? p) (loop p)]
                    [(null? p) '()]
                    [else (syntax-violation #f "invalid formal argument" formals)])]
            [(pair? formals)
                (unless (identifier? (car formals))
                    (syntax-violation #f "invalid formal argument" (car formals)))
                (cons (add-scope (car formals) sc)
                      (loop (cdr formals)))]
            [(null? formals) '()]
            [else (syntax-violation #f "invalid formal argument" formals)])))  