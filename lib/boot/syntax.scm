(let* ()
    (define (runtime-envirionment->syntactic env)
        (cond 
            [(interpreter-environment? env) (%top-level-runtime-senv env)]
            [(environment? env) (%internal-runtime-senv env)]
            [else (assertion-violation #f "unknown environment type" env)]))

    (define (runtime-lookup identifier env)
        (if (syntactic-closure? identifier)
            #f 
            (environment-lookup-macro env identifier)))
    
    (define (senv->runtime senv)
        ((senv-get-runtime senv)))

    (define (senv-top-level? senv)
        (memq ((senv-get-type senv)) '(top-level 'sealed)))
    
    (define (lookup-identifier identifier senv)
        (if (not (identifier? identifier))
            (assertion-violation 'lookup-identifier "not an identifier" identifier))
        (let loop ([id identifier] [senv senv])
            (or ((senv-lookup senv) id)
                (if (syntactic-closure? id)
                    (loop (syntactic-closure-form id) (syntactic-closure-senv id))
                    (make-toplevel-ref #f '() id)))))
    
    (define (reserve-keyword identifier senv)
        ((senv-store senv) identifier (reserved-name-item)))

    (define (bind-keyword identifier senv item)
        ((senv-store senv) identifier item))
    
    (define (bind-variable identifier senv)
        (let ([rename ((senv-rename senv) identifier)])
            ((senv-store senv) rename ())))




    (define &transformer-item
        (let* ([rtd (make-record-type-descriptor '&transformer-item #f #f #f #f '#((immutable impl) (immutable expr)))]
               [rcd (make-record-constructor-descriptor rtd #f #f)])
            (make-record-type '&transformer-item rtd rcd)))
    (define transformer-item? (record-predicate (record-type-rtd &transformer-item)))
    (define transformer-item-impl (record-accessor (record-type-rtd &transformer-item) 0))
    (define transformer-item-expr (record-accessor (record-type-rtd &transformer-item) 1))
    (define %transformer-item (record-constructor (record-type-rcd &transformer-item)))

    (define default-object (list '(default)))

    (define (transformer-item impl . expr)
        (%transformer-item impl (if (null? expr) default-object (car expr))))

    (define (transformer-impl-has-expr? item)
        (not (eq? (transformer-item-expr item) default-object)))

    (define &classifier-item 
        (let* ([rtd (make-record-type-descriptor '&classifier-item #f #f #f #f '#((immutable impl)))]
               [rcd (make-record-constructor-descriptor rtd #f #f)])
            (make-record-type '&classifier-item rtd rcd)))

    (define classifier-item? (record-predicate (record-type-rtd &classifier-item)))
    (define classifier-item-impl (record-accessor (record-type-rtd &classifier-item) 0))
    (define classifier-item (record-constructor (record-type-rcd &classifier-item)))

    (define &syntactic-closure 
        (let* ([rtd (make-record-type-descriptor '&syntactic-closure #f #f #f #f '#((immutable senv) (immutable free) (immutable form)))]
               [rcd (make-record-constructor-descriptor rtd #f #f)])
            (make-record-type '&syntactic-closure rtd rcd)))
            
    (define %make-syntactic-closure (record-constructor (record-type-rcd &syntactic-closure)))
    (define syntactic-closure? (record-predicate (record-type-rtd &syntactic-closure)))
    (define syntactic-closure-senv (record-accessor (record-type-rtd &syntactic-closure) 0))
    (define syntactic-closure-free (record-accessor (record-type-rtd &syntactic-closure) 1))
    (define syntactic-closure-form (record-accessor (record-type-rtd &syntactic-closure) 2))

    (define (constant-form? form)
        (not (or (syntactic-closure? form)
            (pair? form)
            (identifier? form))))

    (define (identifier? object)
        (or (symbol? object)
            (closed-identifier? object)))
    (define (closed-identifier? object)
        (and (syntactic-closure? object)
            (null? (syntactic-closure-free object))
            (identifier? (syntactic-closure-form object))))

    (define (make-syntactic-closure senv free form)
        (if (or (memq form free)		;LOOKUP-IDENTIFIER assumes this.
            (constant-form? form)
            (and (syntactic-closure? form)
                (null? (syntactic-closure-free form))
                (not (identifier? (syntactic-closure-form form)))))
            form
            (%make-syntactic-closure senv free form)))

    (define (classify-form form senv)
        (cond 
            [(identifier? form)
                (lookup-identifier form senv)]
            [(syntactic-closure? form)
                (reclassify (syntactic-closure-form form)
                            (make-partial-senv (syntactic-closure-free form)
                                               senv
                                               (syntactic-closure-senv form)))]
            [(pair? form)
                (define item (classify-form (car form) senv))

                (cond 
                    [(classifier-item? item)
                        (apply-classifier-item item form senv)]
                    [(transformer-item? item)
                        (define impl (transformer-item-impl item))
                        (let ([form* (impl form senv)])
                            (reclassify form* senv))]
                    [else 
                        (if (not (list? form))
                            (assertion-violation #f "combination must be a proper list" form))
                        (combination-item ctx)
                    ]
                )
            ]
        ))
)
