(define :expand-context 
    (let* ([rtd (make-record-type-descriptor 'expand-context #f #f #f #f
        '#(
            (immutable scopes)
            (immutable use-site-scopes)
            (immutable module-scopes)
            (immutable context)
            (immutable phase)
            (immutable namespace)
            (immutable env)
            (immutable only-immediate?)
            (immutable post-expansion-scope)
            (immutable module-begin-k)))]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'expand-context rtd rcd)))

(define expand-context? (record-predicate (record-type-rtd :expand-context)))
(define expand-context-scopes (record-accessor (record-type-rtd :expand-context) 0))
(define expand-context-use-site-scopes (record-accessor (record-type-rtd :expand-context) 1))
(define expand-context-module-scopes (record-accessor (record-type-rtd :expand-context) 2))
(define expand-context-context (record-accessor (record-type-rtd :expand-context) 3))
(define expand-context-phase (record-accessor (record-type-rtd :expand-context) 4))
(define expand-context-namespace (record-accessor (record-type-rtd :expand-context) 5))
(define expand-context-env (record-accessor (record-type-rtd :expand-context) 6))
(define expand-context-only-immediate? (record-accessor (record-type-rtd :expand-context) 7))
(define expand-context-post-expansion-scope (record-accessor (record-type-rtd :expand-context) 8))
(define expand-context-module-begin-k (record-accessor (record-type-rtd :expand-context) 9))

(define %make-expand-context (record-constructor (record-type-rcd :expand-context)))

(define (make-expand-context ns)
    (%make-expand-context
        '() ; scope
        #f  ; use-site scopes
        (list (namespace-scope ns)) ; module scopes
        'top-level
        0
        ns
        empty-env
        #f 
        #f 
        #f))

(define current-expand-context (make-parameter #f))