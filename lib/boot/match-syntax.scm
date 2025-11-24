

    (define (match-syntax orig-s pattern . error?)
        (define error (if (null? error?) (lambda (msg irritants) (syntax-violation #f msg irritants)) (car error?)))

        (define (match s pattern)
            (cond 
                [(symbol? pattern)
                    (list (list pattern s))]
                [(syntax? s)
                    (match (syntax-expression s) pattern)]
                [(and (list? pattern)
                    (= (length pattern) 2)
                    (or (eq? '::: (cadr pattern))
                        (eq? ':::+ (cadr pattern))))
                    (let* ([flat-s (to-syntax-list s)])
                        (cond 
                            [(null? flat-s)
                                (when (eq? ':::+ (cadr pattern))
                                    (error "bad syntax" orig-s))
                                (make-empty-vars pattern)]
                            [(list? flat-s)
                                (let ([a-lists (map (lambda (x) (match x (car pattern))) flat-s)]) 
                                    (apply map
                                        (lambda slice 
                                            (list (caar slice)
                                                  (map cadr slice)))
                                        a-lists))]
                            [else (error "bad syntax:" orig-s)]))]
                [(pair? pattern)
                    (cond 
                        [(pair? s)
                            (append (match (car s) (car pattern))
                                    (match (cdr s) (cdr pattern)))]
                        [else (error "bad syntax:" orig-s)])]
                [(null? pattern)
                    (cond 
                        [(null? s) '()]
                        [else (error "bad syntax:" orig-s)])]
                [else (error "bad pattern")]))

        (define a-list (match orig-s pattern))
        (lambda (sym)
            (define a (assq sym a-list))
            (if a 
                (cadr a)
                (error "no such variable in pattern:" sym))
            ))

    (define (make-empty-vars pattern)
      (cond
      [(symbol? pattern)
        (list (list pattern null))]
      [(and (list? pattern)
            (= (length pattern) 2)
            (or (eq? '::: (cadr pattern))
                (eq? ':::+ (cadr pattern))))
        (map (lambda (m)
              (cons (car m) (list (cadr m))))
            (make-empty-vars (car pattern)))]
      [(pair? pattern)
        (append (make-empty-vars(car pattern))
                (make-empty-vars(cdr pattern)))]
      [else
        '()]))

    (define (try-match-syntax orig-s pattern)
        (call/cc 
            (lambda (exit)
                (match-syntax orig-s pattern 
                    (lambda args (exit #f))))))

    (define (to-syntax-list s)
        (cond 
            [(pair? s) (cons (car s) (to-syntax-list (cdr s)))]
            [(syntax? s) (to-syntax-list (syntax-expression s))]
            [else s]))