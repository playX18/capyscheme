(define :syntax
    (let* ([rtd (make-record-type-descriptor 'syntax #f #f #f #f '#((immutable e) (immutable scopes) (immutable shifted-multi-scopes) (immutable srcloc) (immutable props)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'syntax rtd rcd)))

(define syntax? (record-predicate (record-type-rtd :syntax)))
(define syntax-e (record-accessor (record-type-rtd :syntax) 0))
(define syntax-scopes (record-accessor (record-type-rtd :syntax) 1))
(define syntax-shifted-multi-scopes (record-accessor (record-type-rtd :syntax) 2))
(define syntax-srcloc (record-accessor (record-type-rtd :syntax) 3))
(define syntax-props (record-accessor (record-type-rtd :syntax) 4))
(define make-syntax (record-constructor (record-type-rcd :syntax)))

(define empty-scopes '())
(define empty-shifted-multi-scopes '())
(define empty-props (make-core-hash-eq))

(define empty-syntax 
    (make-syntax #f empty-scopes empty-shifted-multi-scopes #f empty-props))

(define (identifier? s)
    (and (syntax? s)
         (symbol? (syntax-e s))))

(define (syntax->datum s)
    (let loop ([s (syntax-e s)])
        (cond 
            [(syntax? s) (loop (syntax-e s))]
            [(pair? s) (cons (loop (car s)) (loop (cdr s)))]
            [(vector? s) 
                (let* ([len (vector-length s)]
                       [vec (make-vector len #f)])
                    (let lp ([i 0])
                        (cond 
                            [(< i len) 
                                (vector-set! vec i (loop (vector-ref s i)))
                                (lp (+ i 1))]
                            [else vec])))]
            [else s])))

(define (datum->syntax stx-c s . rest)
    (define stx-l
        (cond 
            [(null? rest) #f]
            [else (car rest)]))
    (define stx-p 
        (cond 
            [(null? rest) #f]
            [(null? (cdr rest)) #f]
            [else (cadr rest)]))
    (define (wrap e)
        (make-syntax e 
                     (if stx-c 
                         (syntax-scopes stx-c)
                         empty-scopes)
                     (if stx-c 
                         (syntax-shifted-multi-scopes stx-c)
                         empty-shifted-multi-scopes)
                     (and stx-l (syntax-srcloc stx-l))
                     (if stx-p (syntax-props stx-p) empty-props)))
    (cond 
        [(syntax? s) s]
        [(pair? s)
            (wrap (cons (datum->syntax stx-c (car s) stx-l stx-p)
                        (datum->syntax stx-c (cdr s) stx-l stx-p)))]
        [(vector? s)
            (let* ([len (vector-length s)]
                   [vec (make-vector len #f)])
                (let lp ([i 0])
                    (cond 
                        [(< i len) 
                            (vector-set! vec i (datum->syntax stx-c (vector-ref s i) stx-l stx-p))
                            (lp (+ i 1))]
                        [else (wrap vec)])))]
        [else (wrap s)]))

(define (syntax-property s key . val?)
    (cond 
        [(null? val?) 
            (unless (syntax? s)
                (assertion-violation 'syntax-property "expected syntax" s))
            (core-hash-ref (syntax-props s) key #f)]
        [else 
            (let ([val (car val?)])
                (make-syntax 
                    (syntax-e s)
                    (syntax-scopes s)
                    (syntax-shifted-multi-scopes s)
                    (syntax-srcloc s)
                    (core-hash-set (syntax-props s) key val)))]))

