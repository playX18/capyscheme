
(define syntax-content #f)

(let* ()
    (define (datum-has-elements? v)
        (or (pair? v) (vector? v) (core-hashtable? v)))

    (define (taint? v)
        (or (not v) (symbol? v)))
    
    (define (tainted-for-content v)
        (if (datum-has-elements? v)
            'tainted/need-propagate
            'tainted))

    (define (tainted-needs-propagate? v)
        (eq? v 'tainted/need-propagate))

    (define (taint-propagated t)    
        (if (eq? t 'tainted/need-propagate)
            'tainted
            t))

    (define &stx 
        (let* ([rtd (make-record-type-descriptor '&syntax #f #f #f #f '#((mutable content*) (immutable scopes) (immutable shifted-multi-scopes) (immutable mpi-shifts) (immutable srcloc) (immutable props)))]
               [rcd (make-record-constructor-descriptor rtd #f #f)])
            (make-record-type '&syntax rtd rcd)))

    (define stx? (record-predicate (record-type-rtd &stx)))
    (define stx-content* (record-accessor (record-type-rtd &stx) 0))
    (define set-stx-content*! (record-mutator (record-type-rtd &stx) 0))
    (define stx-scopes (record-accessor (record-type-rtd &stx) 1))
    (define stx-shifted-multi-scopes (record-accessor (record-type-rtd &stx) 2))
    (define stx-mpi-shifts (record-accessor (record-type-rtd &stx) 3))
    (define stx-srcloc (record-accessor (record-type-rtd &stx) 4))
    (define stx-props (record-accessor (record-type-rtd &stx) 5))
   
    (define make-stx (record-constructor (record-type-rcd &stx)))

    (define &modified-content 
        (let* ([rtd (make-record-type-descriptor '&modified-content #f #f #f #f '#((immutable content) (immutable scope-propagations+taint)))]
               [rcd (make-record-constructor-descriptor rtd #f #f)])
            (make-record-type '&modified-content rtd rcd)))
    
    (define modified-content? (record-predicate (record-type-rtd &modified-content)))
    (define modified-content-content (record-accessor (record-type-rtd &modified-content) 0))
    (define modified-content-scope-propagations+taint (record-accessor (record-type-rtd &modified-content) 1))
    (define make-modified-content (record-constructor (record-type-rcd &modified-content)))

    (define (stx-content s)
        (let ([c (stx-content* s)])
            (if (modified-content? c)
                (modified-content-content c)
                c)))
    (define (stx-taintness s)
        (let ([c (stx-content* s)])
            (cond 
                [(modified-content? c)
                    (let ([v (modified-content-scope-propagations+taint c)])
                        (if (pair? v) (cdr v) #f))]
                ]
            )))
)