(define-library (cffi early-types)
    (import (rnrs)
            (rnrs hashtables)
            (rnrs records syntactic (6))
            (core optargs))
(begin 
    (define *default-type-parsers* (make-eq-hashtable))
    (define *struct-type-parsers* (make-eq-hashtable))
    (define *union-type-parsers* (make-eq-hashtable))

    (define (namespace-table namespace)
        (case 
            [(default) *default-type-parsers*]
            [(struct) *struct-type-parsers*]
            [(union) *union-type-parsers*]))

    (define (find-type-parser type-name . args)
        (let-optionals args ([namespace 'default])
            (or (hashtable-ref (namespace-table namespace) type-name)
                (assertion-violation #f "undefined foreign type: ~a in namespace ~a"
                    type-name namespace))))

    (define (find-default-type-parser type-name)
        (find-type-parser type-name 'namespace 'default))

    (define (register-type-parser! type-name parser . args)
        (let-optionals args ([namespace 'default])
            (hashtable-set! (namespace-table namespace) type-name parser)))
            
    (define (unregister-foreign-type! type-name . args)
        (let-optionals args ([namespace 'default])
            (hashtable-remove! (namespace-table namespace) type-name)))

    (define-syntax define-parse-method
        (syntax-rules () 
            [(_ type-name args body ...)
                (register-type-parser! 'type-name 
                    (lambda args 
                        body ...))]))
    
    (define (notice-foreign-type type-name . args)
        (let-optionals args ([namespace 'default])
            (hashtable-set! (namespace-table namespace) type-name #t)
            type-name))
            
    

    
            
))