
(define core-scope (new-multi-scope))

(define core-stx (add-scope empty-syntax core-scope))

(define core-forms (make-core-hash-eq))
(define core-primitives (make-core-hash-eq))

(define (add-core-form! sym proc)
    (add-core-binding! sym)
    (core-hash-set! core-forms sym proc))

(define (add-core-primitive! sym val)
    (add-core-binding! sym)
    (core-hash-set! core-primitives sym val))

(define (add-core-binding! sym)
    (add-binding! (datum->syntax core-stx sym)
                  (make-module-binding 'core 0 sym 'core 0 sym 0)
                  0))

    
(define (declare-core-module! ns)
    (define self-name 'core)
    (define requires (make-core-hash-eq))
    (define provides (make-core-hash-eqv))
    (define min-phase-level 0)
    (define max-phase-level 1)
    (define (instantiate ns phase phase-level)
        (case phase-level 
            [(0) 
                (for-each
                    (lambda (entry)
                        (namespace-set-variable!
                            ns
                            0
                            (car entry)
                            (cdr entry)))
                    (core-hash->list core-primitives))]
            [(1)
                (for-each 
                    (lambda (entry)
                        (namespace-set-transformer!
                            ns
                            1
                            (car entry)
                            (make-core-form (cdr entry))))
                    (core-hash->list core-forms))]))
    (declare-module! 
        ns 
        (make-mod 
            self-name
            requires
            provides
            min-phase-level
            max-phase-level
            instantiate)))

(define (core-form-sym s phase)
    (define m (try-match-syntax s '(id . _)))
    (and m 
        (let ([b (resolve (m 'id) phase)])
            (and (module-binding? b)
                 (eq? 'core (module-binding-module b)
                 (module-binding-sym b))))))

