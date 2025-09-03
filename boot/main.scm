(define record-type-vtable 
    (let ((s (make-vtable (string-append standard-vtable-fields "pwpwpwpwpwpw") #f)))
        (set-struct-vtable-name! s 'record-type)
        s))

(define (record-type? obj)
    (if (struct? obj)
        (eq? record-type-vtable (struct-vtable obj))
        #f))

(define (record-type-name rtd)
    (struct-ref rtd vtable-offset-user))

(define (record-type-fields rtd)
    (struct-ref rtd (+ 1 vtable-offset-user)))

(define (record-type-constructor rtd)
    (struct-ref rtd (+ 2 vtable-offset-user)))

(define (record-type-properties rtd)
    (struct-ref rtd (+ 3 vtable-offset-user)))

(define (record-type-parents rtd)
    (struct-ref rtd (+ 4 vtable-offset-user)))