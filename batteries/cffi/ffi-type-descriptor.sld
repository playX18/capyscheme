(define-library (cffi ffi-type-descriptor)
    (import (rnrs)
            (rnrs records syntactic (6)))
(begin 
    (define-record-type type-descriptor 
        (fields name size))
    
    (define-record-type ffi-type-descriptor 
        (parent type-descriptor)
        (fields alias)
        (protocol 
            (lambda (n)
                (lambda (name alias size)
                    ((n name size) alias)))))
    
    (define-record-type pointer-accessibe-ffi-type-descriptor 
        (parent ffi-type-descriptor)
        (fields 
            pointer-ref 
            pointer-set!)
        (protocol
            (lambda (n)
                (lambda (name alias size ref set)
                    ((n name alias size) ref set)))))

    (define-record-type foreign-struct-descriptor
        (parent type-descriptor)
        (fields 
            fields
            parent
            (mutable protocol)
            has-protocol?
            (mutable ctr)
            (mutable getters)
            (mutable setters))
        (protocol (lambda (p)
                    (lambda (name size fields parent protocol)
                        ((p name size) fields parent #f protocol #f '() '())))))

    (define-record-type generic-foreign-struct-descriptor
        (parent foreign-struct-descriptor)
        (fields alignment type-ref type-set!)
        (protocol (lambda (n)
                    (lambda (name size alignment fields parent proto ref set)
                        ((n name size fields parent proto)
                alignment ref set)))))

))