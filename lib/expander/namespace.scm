(define :namespace
    (let* ([rtd (make-record-type-descriptor 'namespace #f #f #f #f '#(
            (immutable scope)    ; scope for top-level bindings
            (immutable phases)   ; phase-level -> definitions
            (immutable module-declarations) ; resolved-module-name -> module
            (immutable submodule-declarations) ; resolved-module-name -> module
            (immutable module-instances) ; (cons resolved-module-name phase) -> module instance
            ))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'namespace rtd rcd)))

(define namespace? (record-predicate (record-type-rtd :namespace)))
(define namespace-scope (record-accessor (record-type-rtd :namespace) 0))
(define namespace-phases (record-accessor (record-type-rtd :namespace) 1))
(define namespace-module-declarations (record-accessor (record-type-rtd :namespace) 2))
(define namespace-submodule-declarations (record-accessor (record-type-rtd :namespace) 3))
(define namespace-module-instances (record-accessor (record-type-rtd :namespace) 4))
(define make-namespace (record-constructor (record-type-rcd :namespace)))

(define :definitions 
    (let* ([rtd (make-record-type-descriptor 'definitions #f #f #f #f '#(
            (immutable variables)          ; sym -> val
            (immutable transformers)       ; sym -> val
            (mutable instantiated?)
            ))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'definitions rtd rcd)))

(define definitions? (record-predicate (record-type-rtd :definitions)))
(define definitions-variables (record-accessor (record-type-rtd :definitions) 0))
(define definitions-transformers (record-accessor (record-type-rtd :definitions) 1))
(define definitions-instantiated? (record-accessor (record-type-rtd :definitions) 2))
(define set-definitions-instantiated?! (record-mutator (record-type-rtd :definitions) 2))
(define make-definitions (record-constructor (record-type-rcd :definitions)))

(define :module 
    (let* ([rtd (make-record-type-descriptor 'module #f #f #f #f '#(
            (immutable self-name)           ; symbol used for a self reference
            (immutable requires)            ; phase -> list of resolved-module-name
            (immutable provides)            ; phase-level -> sym -> binding
            (immutable min-phase-level)     ; phase-level
            (immutable max-phase-level)     ; phase-level
            (immutable instantiate)
            ))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'module rtd rcd)))

(define mod? (record-predicate (record-type-rtd :module)))
(define mod-self-name (record-accessor (record-type-rtd :module) 0))
(define mod-requires (record-accessor (record-type-rtd :module) 1))
(define mod-provides (record-accessor (record-type-rtd :module) 2))
(define mod-min-phase-level (record-accessor (record-type-rtd :module) 3))
(define mod-max-phase-level (record-accessor (record-type-rtd :module) 4))
(define mod-instantiate (record-accessor (record-type-rtd :module) 5))
(define make-mod (record-constructor (record-type-rcd :module)))

(define (make-empty-namespace)
    (make-namespace (new-multi-scope) ; top-level scope
                    (make-core-hash-eqv) ; phases
                    (make-core-hash-equal) ; module-declarations
                    (make-core-hash-equal) ; submodule-declarations
                    (make-core-hash-equal) ; module-instances
                    ))

(define current-namespace (make-parameter (make-empty-namespace)))

(define (namespace-syntax-introduce s . ns?)
    (define ns (if (null? ns?) (current-namespace) (car ns?)))
    (add-scope s (namespace-scope ns)))

(define (make-module-namespace ns name for-submodule?)
    (define m-ns 
        (make-namespace 
            (new-multi-scope) ; top-level scope
            (make-core-hash-eqv) ; phases
            (namespace-module-declarations ns) ; module-declarations
            (if (for-submodule?) ; submodule-declarations
                (namespace-submodule-declarations ns)
                (make-core-hash-equal))
            (make-core-hash-equal) ; module-instances
        ))
    (core-hash-set! (namespace-module-instances ns) (cons name 0) m-ns)
    m-ns)

(define (namespace->module ns name)
    (or (core-hash-ref (namespace-submodule-declarations ns) name #f)
        (core-hash-ref (namespace-module-declarations ns) name #f)))



(define (declare-module! ms name m . as-submod?)
    (define as-submodule? (if (null? as-submod?) #f (car as-submod?)))

    (core-hash-set! (if as-submodule? 
        (namespace-submodule-declarations ms)
        (namespace-module-declarations ms))
        name 
        m))

(define (namespace-module-instantiate! ns name phase-shift . min-phase?)
    (define min-phase (if (null? min-phase?) 0 (car min-phase?)))
    (define m-ns (namespace->module-namespace ns name phase-shift #t))
    (define m (namespace->module ns name))
    (define phase-end (+ 1 (mod-max-phase-level m)))
  
    (for-each 
        (lambda (entry)
            (define req-phase (car entry))
            (define mods (cdr entry))
            (for-each 
                (lambda (mod)
                    (namespace-module-instantiate! ns mod (phase+ phase-shift req-phase) min-phase))
                mods))
        (core-hash->list (mod-requires m)))


    (let loop ([phase-level (mod-min-phase-level m)])
        (when (< phase-level phase-end)
            (when (>= (phase+ phase-shift phase-level) min-phase)
                (let* ([phase (phase+ phase-shift phase-level)]
                    [defs (namespace->definitions m-ns phase-level)])
                    (unless (definitions-instantiated? defs)
                        (set-definitions-instantiated?! defs #t)
                        ((mod-instantiate m) m-ns phase-shift phase-level))))
            (loop (+ 1 phase-level)))))

(define (namespace-module-visit! ns name phase)
    (namespace-module-instantiate! ns name phase 1))

(define (namespace->module-namespace ns name phase . create?)
    (define create (if (null? create?) #f (car create?)))
  
    (or (core-hash-ref (namespace-module-instances ns) (cons name phase) #f)
        (and create 
            (let ([m (namespace->module ns name)])
                (unless m 
                    (assertion-violation #f "no module declared to instantiate" name))
                (let ([m-ns (make-namespace (new-multi-scope)
                                            (make-core-hash-eqv)
                                            (namespace-module-declarations ns)
                                            (namespace-submodule-declarations ns)
                                            (namespace-module-instances ns))])
                    (core-hash-set! (namespace-module-instances ns) (cons name phase) m-ns)
                    m-ns)))))

(define (namespace->definitions ns phase-level)
    (define d (begin 
       
        (core-hash-ref (namespace-phases ns) phase-level #f)))
    
    (or d 
        (let ([d (make-definitions (make-core-hash-eq) (make-core-hash-eq) #f)])
            (core-hash-set! (namespace-phases ns) phase-level d)
            d)))

(define (namespace-set-variable! ns phase-level name val)
    (define d (namespace->definitions ns phase-level))
    (core-hash-set! (definitions-variables d) name val))

(define (namespace-set-transformer! ns phase-level name val)
    (define d (namespace->definitions ns phase-level))
    (core-hash-set! (definitions-transformers d) name val))

(define (namespace-get-variable ns phase-level name fail-k)
    (define d (namespace->definitions ns phase-level))
    (define val (core-hash-ref (definitions-variables d) name #f))
    (if val 
        val
        (fail-k)))

(define (namespace-get-transformer ns phase-level name fail-k)
    (define d (namespace->definitions ns phase-level))
    (define val (core-hash-ref (definitions-transformers d) name #f))
   
    (if val 
        val
        (fail-k)))