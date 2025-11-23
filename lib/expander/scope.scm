;; A scope represents a distinct "dimension" of binding. We can attach
;; the bindings for a set of scopes to an arbitrary scope in the set;
;; we pick the most recently allocated scope to make a binding search
;; faster and to improve GC, since non-nested binding contexts will
;; generally not share a most-recent scope.

(define :scope 
    (let* ([rtd (make-record-type-descriptor 'scope #f #f #f #f '#((immutable id) (immutable bindings)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'scope rtd rcd)))

(define scope? (record-predicate (record-type-rtd :scope)))
(define scope-id (record-accessor (record-type-rtd :scope) 0))
(define scope-bindings (record-accessor (record-type-rtd :scope) 1))
(define make-scope (record-constructor (record-type-rcd :scope)))   

(define :multi-scope 
    (let* ([rtd (make-record-type-descriptor 'multi-scope #f #f #f #f '#((immutable scopes)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'multi-scope rtd rcd)))

(define multi-scope? (record-predicate (record-type-rtd :multi-scope)))
(define multi-scope-scopes (record-accessor (record-type-rtd :multi-scope) 0))
(define make-multi-scope (record-constructor (record-type-rcd :multi-scope)))

(define :representative-scope 
    (let* ([rtd (make-record-type-descriptor 'representative-scope (record-type-rtd :scope) #f #f #f '#((immutable owner) (immutable phase)))]
           [rcd (make-record-constructor-descriptor rtd (record-type-rcd :scope) #f)])
        (make-record-type 'representative-scope rtd rcd)))

(define representative-scope? (record-predicate (record-type-rtd :representative-scope)))
(define representative-scope-owner (record-accessor (record-type-rtd :representative-scope) 0))
(define representative-scope-phase (record-accessor (record-type-rtd :representative-scope) 1))
(define make-representative-scope (record-constructor (record-type-rcd :representative-scope)))

(define :shifted-multi-scope 
    (let* ([rtd (make-record-type-descriptor 'shifted-multi-scope #f #f #f #f '#((immutable phase) (immutable multi-scope)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'shifted-multi-scope rtd rcd)))

(define shifted-multi-scope? (record-predicate (record-type-rtd :shifted-multi-scope)))
(define shifted-multi-scope-phase (record-accessor (record-type-rtd :shifted-multi-scope) 0))
(define shifted-multi-scope-multi-scope (record-accessor (record-type-rtd :shifted-multi-scope) 1))
(define make-shifted-multi-scope (record-constructor (record-type-rcd :shifted-multi-scope)))


(define id-counter 0)
(define (new-scope-id!)
    (set! id-counter (+ id-counter 1))
    id-counter)

(define (make-bindings)
    (make-core-hash-eq))

(define (new-scope)
    (make-scope (new-scope-id!) (make-bindings)))

(define (new-multi-scope . name?)
    (define name (cond 
                     [(null? name?) (gensym)]
                     [else (car name?)]))
    (make-shifted-multi-scope 0
        (make-multi-scope (make-core-hash-eqv))))

(define (multi-scope-to-scope-at-phase ms phase)
    (or (core-hash-ref (multi-scope-scopes ms) phase)
        (let ([s (make-representative-scope (new-scope-id!) (make-bindings) ms phase)])
            (core-hash-set! (multi-scope-scopes ms) phase s)
            s)))

(define (scope>? sc1 sc2)
    (> (scope-id sc1) (scope-id sc2)))

(define (adjust-scope s sc op)
    (cond 
        [(syntax? s) (make-syntax 
                            (adjust-scope (syntax-e s) sc op)
                            (if (shifted-multi-scope? sc)
                                (syntax-scopes s)
                                (op (syntax-scopes s) sc))
                            (if (shifted-multi-scope? sc)
                                (op (syntax-shifted-multi-scopes s) sc)
                                (syntax-shifted-multi-scopes s))
                            (syntax-srcloc s)
                            (syntax-props s))]
        [(pair? s) (cons (adjust-scope (car s) sc op)
                         (adjust-scope (cdr s) sc op))]
        [else s]))

(define (generalize-scope sc)
    (if (representative-scope? sc)
        (make-shifted-multi-scope 
            (representative-scope-phase sc)
            (representative-scope-owner sc))
        sc))

(define (add-scope s sc)
    (adjust-scope s (generalize-scope sc) set-add))

(define (add-scopes s scs)
    (foldl (lambda (acc sc)
        (add-scope acc sc))
        s 
        scs))

(define (remove-scope s sc)
    (adjust-scope s (generalize-scope sc) set-remove))

(define (remove-scopes s scs)
    (foldl (lambda (acc sc)
        (remove-scope acc sc))
        s 
        scs))

(define (set-flip s e)
    (if (set-member? s e)
        (set-remove s e)
        (set-add s e)))

(define (flip-scope s sc)
    (adjust-scope s (generalize-scope sc) set-flip))

(define (phase? v)
    (or (not v)
        (exact-integer? v)))

(define (phase+ a b)
    (and a b (+ a b)))

(define (phase- a b)
    (and a b (- a b)))

(define (shift-multi-scope sms delta)
    (if (zero? delta)
        sms 
        (make-shifted-multi-scope 
            (phase+ delta (shifted-multi-scope-phase sms))
            (shifted-multi-scope-multi-scope sms))))

(define (syntax-shift-phase-level s phase)
    (if (eqv? phase 0)
        s 
        (let loop ([s s])
            (cond 
                [(syntax? e)
                    (make-syntax 
                        (loop (syntax-e s))
                        (syntax-scopes s)
                        (map (lambda (sms)
                            (shift-multi-scope sms phase))
                            (syntax-shifted-multi-scopes s))
                        (syntax-srcloc s)
                        (syntax-props s))]
                [(pair? e) (cons (loop (car s))
                             (loop (cdr s)))]
                [else s]))))

(define (syntax-scope-set s phase)
    "Assemble the complete set of scopes at a given phase by extracting
    a phase specific representative from each multi-scope"

    (foldl 
        (lambda (scopes sms)
            (set-add scopes 
                (multi-scope-to-scope-at-phase 
                    (shifted-multi-scope-multi-scope sms)
                    (phase- (shifted-multi-scope-phase sms) phase))))
        (syntax-scopes s)
        (syntax-shifted-multi-scopes s)))

(define (add-binding-in-scopes! scopes sym binding)
    (define max-sc (foldl 
            (lambda (max-sc sc)
                (if (scope>? sc max-sc) sc max-sc))
            (car scopes)
            scopes))
    
    
    (define bindings (scope-bindings max-sc))
    (define sym-bindings (or (core-hash-ref bindings sym)
                             (let ([new-set (make-core-hash-equal)])
                                 (core-hash-set! bindings sym new-set)
                                 new-set)))

    

    (core-hash-set! sym-bindings scopes binding))

(define (add-binding! id binding phase)
    (if (null? (syntax-scope-set id phase))
        (error #f "cannot add binding to identifier with no scopes:" id))
    (add-binding-in-scopes! (syntax-scope-set id phase) (syntax-e id) binding))

(define (resolve s phase . exct?)
    (define exactly? (if (null? exct?) #f (car exct?)))
    (unless (identifier? s)
        (assertion-violation 'resolve "expected identifier" s))
    (unless (phase? phase)
        (assertion-violation 'resolve "expected phase" phase))
    
    (let* ([scopes (syntax-scope-set s phase)]
           [candidates (find-all-matching-bindings s scopes)])
        (cond 
            [(pair? candidates)
                (let ([max-candidate (argmax (lambda (c) (length (car c))) candidates)])
                    (and (or (not exactly?)
                             (equal? (length scopes)
                                     (length (car max-candidate))))
                         (cdr max-candidate)))]
            [else #f])))

(define (find-all-matching-bindings s scopes)
    (define sym (syntax-e s))
    (let loop ([scopes scopes] [acc '()])
        (cond 
            [(null? scopes)
                (reverse acc)]
            [else 
                (let* ([sc (car scopes)]
                       [bindings (core-hash-ref (scope-bindings sc) sym #f)])
                    (loop 
                        (cdr scopes)

                        (cond 
                            [(not bindings) acc]
                            [else 
                                (let inner ([entries (core-hash->list bindings)]
                                            [acc acc])
                                    (cond 
                                        [(null? entries) acc]
                                        [else
                                            (let* ([entry (car entries)]
                                                   [b-scopes (car entry)]
                                                   [binding (cdr entry)])
                                                (inner (cdr entries)
                                                    (if (set-sbuset? b-scopes scopes)
                                                        (cons (cons b-scopes binding) acc)
                                                        acc)))]))])))])))

(define (bound-identifier=? a b phase)
    (and (eq? (syntax-e a)
              (syntax-e b))
         (equal? (syntax-scope-set a phase)
                 (syntax-scope-set b phase))))

