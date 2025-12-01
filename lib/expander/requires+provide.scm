(define :requires+provides
    (let* ([rtd (make-record-type-descriptor 'requires+provider #f #f #f #f '#(
        (immutable requires)
        (immutable provides)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'requires+provides rtd rcd)))

(define requires+provides? (record-predicate (record-type-rtd :requires+provides)))
(define requires+provides-requires (record-accessor (record-type-rtd :requires+provides) 0))
(define requires+provides-provides (record-accessor (record-type-rtd :requires+provides) 1))
(define %make-requires+provides (record-constructor (record-type-rcd :requires+provides)))

(define (make-requires+provides)
    (%make-requires+provides (make-core-hash-equal) (make-core-hash-eqv)))

(define :required
    (let* ([rtd (make-record-type-descriptor 'required #f #f #f #f '#((immutable id) (immutable phase) (immutable can-shadow?)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'required rtd rcd)))

(define required? (record-predicate (record-type-rtd :required)))
(define required-id (record-accessor (record-type-rtd :required) 0))
(define required-phase (record-accessor (record-type-rtd :required) 1))
(define required-can-shadow? (record-accessor (record-type-rtd :required) 2))
(define make-required (record-constructor (record-type-rcd :required)))

(define (add-required-module! r+p mod-name phase-shift)
    (define at-mod (core-hash-ref (requires+provides-requires r+p) mod-name (make-core-hash-eqv)))
    (unless (core-hash-contains? at-mod phase-shift)
        (core-hash-set! at-mod phase-shift '()))
    (core-hash-set! (requires+provides-requires r+p) mod-name at-mod))

(define (add-defined-or-required-id! r+p id phase binding . can-shadow)
    (define can-shadow? (if (null? can-shadow) #f (car can-shadow)))
    (unless (equal? phase (phase+ (module-binding-nominal-phase binding)
                                  (module-binding-nominal-require-phase binding)))
        (syntax-violation #f "binding phase does not match required phase" id))
    (let ()
    (define at-mod (core-hash-ref (requires+provides-provides r+p) (module-binding-nominal-module binding) (make-core-hash-eqv)))
    (define l (core-hash-ref at-mod (module-binding-nominal-require-phase binding) '()))
    (core-hash-set! at-mod phase (cons (make-required id phase can-shadow?) l))))


(define (check-not-required-or-defined r+p id phase)
    (define b (resolve id phase #t))
    (when b
        (let ([at-mod (core-hash-ref (requires+provides-provides r+p) (module-binding-nominal-module b) #f)])
            (and 
                at-mod 
                (for-each
                    (lambda (r)
                        (when (and (eq? (syntax-e id) (syntax-e (required-id r)))
                                   (not (required-can-shadow? r)))
                            (syntax-violation #f "identifier already defined or required" id))
                    )
                    (core-hash-ref at-mod (module-binding-nominal-require-phase b) '()))))))

(define (extract-module-requires r+p mod-path phase)
    (define at-mod (core-hash-ref (requires+provides-requires r+p) mod-path #f))
    (and at-mod 
        (core-hash-ref at-mod phase #f)))

(define (reset-provides! r+p)
    (core-hash-clear! (requires+provides-provides r+p)))

(define (add-provide! r+p sym phase binding id)
    (define at-phase (core-hash-ref (requires+provides-provides r+p) phase (make-core-hash-eq)))
    (define b (core-hash-ref at-phase sym #f))
    (cond 
        [(not b)
            (core-hash-set! at-phase sym binding)]
        [(and (equal? (module-binding-module b) (module-binding-module binding))
              (eqv? (module-binding-phase b) (module-binding-phase binding))
              (eq? (module-binding-sym b) (module-binding-sym binding)))
            (unspecified)]
        [else 
            (syntax-violation #f "name already provided as a different binding" sym)])
    (core-hash-set! (requires+provides-provides r+p) phase at-phase))

(define (attach-require-provide-properties r+p s self)
    (define (extract-requires)
        (define mht
            (foldl 
                (lambda (mht entry)
                    (define module-name (car entry))
                    (define ht (cdr entry))
                    (foldl 
                        (lambda (mht phase)
                            (if (eq? module-name self)
                                mht
                                (begin 
                                    (let ([reqs (core-hash-ref ht phase '())])
                                        (core-hash-set mht phase (set-add reqs module-name))
                                    mht))))
                        mht
                        (map car (core-hash->list ht))))
                (make-core-hash-eqv)
                (core-hash->list (requires+provides-requires r+p))))
        (define res (make-core-hash-eqv))
        (for-each
            (lambda (entry)
                (define phase (car entry))
                (define mods (cdr entry))
                (core-hash-set! res phase mods))
            (core-hash->list mht))
        res)
            
    (let* ([s (syntax-property s 'module-requires (extract-requires))]
           [s (syntax-property s 'module-provides (requires+provides-provides r+p))])
        s))