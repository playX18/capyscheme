(define :module-binding 
    (let* ([rtd (make-record-type-descriptor 'module-binding #f #f #f #f '#(
        (immutable module)
        (immutable phase)
        (immutable sym)
        (immutable nominal-module)
        (immutable nominal-phase)
        (immutable nominal-sym)
        (immutable nominal-require-phase)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'module-binding rtd rcd)))

(define module-binding? (record-predicate (record-type-rtd :module-binding)))
(define module-binding-module (record-accessor (record-type-rtd :module-binding) 0))
(define module-binding-phase (record-accessor (record-type-rtd :module-binding) 1))
(define module-binding-sym (record-accessor (record-type-rtd :module-binding) 2))
(define module-binding-nominal-module (record-accessor (record-type-rtd :module-binding) 3))
(define module-binding-nominal-phase (record-accessor (record-type-rtd :module-binding) 4))
(define module-binding-nominal-sym (record-accessor (record-type-rtd :module-binding) 5))
(define module-binding-nominal-require-phase (record-accessor (record-type-rtd :module-binding) 6))
(define make-module-binding (record-constructor (record-type-rcd :module-binding)))


;; Represent a local binding with a key, where the value of
;; the key is kept in a separate environment. That indirection
;; ensures that a fuly expanded program doesn't reference
;; compile-time values from local bindings, but it records that
;; the binding was local.
(define :local-binding 
    (let* ([rtd (make-record-type-descriptor 'local-binding #f #f #f #f '#((immutable key)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'local-binding rtd rcd)))

(define local-binding? (record-predicate (record-type-rtd :local-binding)))
(define local-binding-key (record-accessor (record-type-rtd :local-binding) 0))
(define make-local-binding (record-constructor (record-type-rcd :local-binding)))

(define (free-identifier=? a b phase)
    (define ab (resolve a phase))
    (define bb (resolve b phase))
    
    (cond 
        [(module-binding? ab)
         (and (module-binding? bb)
              (eq? (module-binding-sym ab)
                   (module-binding-sym bb))
              (equal? (module-binding-phase ab)
                      (module-binding-phase bb))
              (equal? (module-binding-module ab)
                      (module-binding-module bb)))]
        [(local-binding? ab)
         (and (local-binding? bb)
              (eq? (local-binding-key ab)
                   (local-binding-key bb)))]
        [else 
            (and (not ab)
                 (not bb)
                 (eq? (syntax-e a) (syntax-e b)))]))

(define (add-local-binding! id phase)
    (define key (gensym (symbol->string (syntax-e id))))
    (add-binding! id (make-local-binding key) phase)
    key)

(define empty-env (make-core-hash-eq))
(define (env-extend env key val)
    (core-hash-set env key val))

;; `variable` is a token to represent a binding to a run-time variable
(define variable (gensym "variable"))
(define (var? t) (eq? t variable))

;; `missing` is a token to represent the absence of a binding; a
;; distinct token is needed so that it's distinct from all compile-time
;; values
(define missing (gensym "missing"))
(define (missing? t) (eq? t missing))

;; A subset of compile-time values are macro transformers
(define (transformer? t) (procedure? t))

;; A subset of compile-time values are primitive forms
(define :core-form 
    (let* ([rtd (make-record-type-descriptor 'core-form #f #f #f #f '#((immutable expander)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'core-form rtd rcd)))

(define core-form? (record-predicate (record-type-rtd :core-form)))
(define core-form-expander (record-accessor (record-type-rtd :core-form) 0))
(define make-core-form (record-constructor (record-type-rcd :core-form)))

;; Returns `variable`, a compile-time value, or `missing` (only
;; in the case of module-level bindings)
(define (binding-lookup b env ns phase id)
    (cond 
        [(module-binding? b)
          
            (let ([m (namespace->module-namespace ns 
                                                  (module-binding-module b)
                                                  (- phase (module-binding-phase b)))])
                (namespace-get-transformer m (module-binding-phase b) (module-binding-sym b) (lambda () variable)))]
        [(local-binding? b)
         
            (let ([t (core-hash-ref env (local-binding-key b) missing)])
                (when (eq? t missing)
                    (syntax-violation #f "identifier used out of context" id))
                t)]
        [else (syntax-violation #f "unknown binding for lookup" id)]))