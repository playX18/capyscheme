
(define current-toplevel-environment (make-parameter #f))
(define current-meta-environment (make-parameter #f))

    (define (make-toplevel-environment renamer)
      (make-environment #f '() renamer))
    (define (make-environment base frame renamer) (tuple 'type:env base frame renamer))
    (define (environment? obj) (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:env)))
    (define (env-base env) (tuple-ref env 1))
    (define (env-frame env) (tuple-ref env 2))
    (define (env-renamer env) (tuple-ref env 3))

    (define (set-env-frame! env frame) (tuple-set! env 2 frame))
    (define (toplevel-env? env) (not (env-base env)))
    (define (with-toplevel-environment top-env thunk)
        (let ([old (current-toplevel-environment)])
            (dynamic-wind 
                (lambda () (current-toplevel-environment top-env))
                thunk
                (lambda () (current-toplevel-environment old)))))
    
    (define (assq-environment id env)
      (let ((frame (env-frame env)))
	(or (assq id frame)
	    (if (toplevel-env? env)
		(and (symbol? id)
		     (let ((new-name ((env-renamer env) id)))
		       (set-env-frame! env (acons id new-name frame))
		       (assq-environment id env)))
		(assq-environment id (env-base env))))))

    (define (install-toplevel-binding! id name top-env)
        (let ([frame (env-frame top-env)])
            (set-env-frame! top-env (acons id name frame))))
    

    (define (make-syntactic-closure env free form) (tuple 'type:syntactic-closure env free form))
    (define (syntactic-closure? obj) (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:syntactic-closure)))
    (define (syntactic-closure-environment sc) (tuple-ref sc 1))
    (define (syntactic-closure-free-names sc) (tuple-ref sc 2))
    (define (syntactic-closure-form sc) (tuple-ref sc 3))

    (define (close-syntax form env) (make-syntactic-closure env '() form))
    (define (unwrap-syntax obj)
        (cond
            [(syntactic-closure? obj) (syntactic-closure-form obj)]
            [(pair? obj) (cons (unwrap-syntax (car obj)) (unwrap-syntax (cdr obj)))]
            [(vector? obj) (list->vector (map unwrap-syntax (vector->list obj)))]
            [else obj]))

    (define (make-id id env) (close-syntax id env))
    (define (id? x) (or (symbol? obj)
	  (and (syntactic-closure? obj)
	       (identifier? (syntactic-closure-form obj)))))
    (define (id=? id1 env1 id2 env2)
        (eq? (expand id1 env1) (expand id2 env2)))
    (define (identifier? obj)
      (or (symbol? obj)
	  (and (syntactic-closure? obj)
	       (identifier? (syntactic-closure-form obj)))))
    (define (identifier=? id1 env1 id2 env2) (eq? (expand id1 env1) (expand id2 env2)))
    (define (make-identifier id env) (make-id id env))
    (define (generate-name id)
        (module-gensym id))

    (define (extend-env! id env)
        (if (not (and (toplevel-env? env) (symbol? id)))
            (let ([frame (env-frame env)])
                (cond
                    [(assq id frame) (syntax-violation #f "duplicate binding" id)]
                    [else 
                        (let ([name (generate-name id)])
                            (set-env-frame! env (acons id name frame))
                            name)]))))
    (define (extend-env ids env)
        (let ([new-env (make-env env '() #f)])
            (for-each 
                (lambda (id)
                    (extend-env! id new-env))
                ids) 
            new-env))
    
    (define (make-expander transformer environment) (tuple 'type:expander transformer environment))
    (define (expander? obj) (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:expander)))
    (define (expander-transformer exp) (tuple-ref exp 1))
    (define (expander-environment exp) (tuple-ref exp 2))

    (define (install-expander! keyword expander env)
        (extend-env! keyword env)
        (let ([cell (assq-environment keyword env)])
            (set-cdr! cell expander)))

    (define (with-meta-environment meta-env thunk)
        (let ([old (current-meta-environment)])
            (dynamic-wind 
                (lambda () (current-meta-environment meta-env))
                thunk
                (lambda () (current-meta-environment old)))))
    
    (define expand 
        (let ()
            (define (expand form env)
                (print "expand form=" form " env=" env)
                (let expand ([form form])
                    (print "expand form=" form " env=" env)
                    (cond 
                        [(identifier? form) (expand-identifier form env)]
                        [(syntactic-closure? form) (expand-syntactic-closure form env)]
                        [(and (pair? form) (list? form))
                            (print "match form=" form)
                            (if (identifier? (car form))
                                (let ([e (expand (car form))])
                                    (print "e=" e)
                                    (if (expander? e)
                                        (expand-macro e form env)
                                        (cons e (map expand (cdr form)))))
                                (map expand form))]
                        [(not (pair? form)) form]
                        [else (syntax-violation #f "invalid syntax" form)])))
            (define (expand-macro expander form env)
                (let ([transformer (expander-transformer expander)]
                      [meta-env (expander-environment expander)])
                    (with-meta-environment meta-env 
                        (lambda () (transformer form env)))))
            (define (expand-syntactic-closure sc env)
                (expand 
                    (syntactic-closure-form sc)
                    (make-environment
                        (syntactic-closure-environment sc)
                        (map (lambda (id) (cons id (expand id env))) (syntactic-closure-free-names sc))
                        #f)))
            
            (define (expand-identifier id env)
                (let ([x (assq-environment id env)])
                    (print "expand-identifier id=" id " env=" env " x=" x)
                    (if x (cdr x)
                        (expand-identifier
                            (syntactic-closure-form id)
                            (syntactic-closure-environment id)))))
                            
        (lambda (form . rest)
            (if (null? rest)
                (expand form (current-toplevel-environment))
                (expand form (car rest))))))
    (define (sc-macro-transformer proc)
        (lambda (form env)
            (expand (proc form env) (current-meta-environment))))
    
    (define (rsc-macro-transformer proc)
        (lambda (form env)
            (expand (proc form (current-meta-environment)) env)))
    (define (er-macro-trasformer proc)
        (lambda (form env)
            (let ([table '()])
                (let (
                    [rename (lambda (x)
                        (let ([v (assq x table)])
                            (if v (cdr v)
                                (let ([id (make-identifier x (current-meta-environment))])
                                    (set! table (acons x id table))
                                    id))))]
                    [compare (lambda (x y) (id=? x env y env))])
                (expand (proc form rename compare) env)))))