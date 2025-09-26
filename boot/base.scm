(define-syntax swap! 
    (lambda (stx)
        (syntax-case stx ()
            [(_ a b) #'(let ([tmp a])
                (set! a b)
                (set! b tmp))])))


(define-syntax else
  (lambda (x)
    (syntax-violation 'else "bad use of 'else' syntactic keyword" x x)))

(define-syntax =>
  (lambda (x)
    (syntax-violation '=> "bad use of '=>' syntactic keyword" x x)))

(define-syntax ...
  (lambda (x)
    (syntax-violation '... "bad use of '...' syntactic keyword" x x)))

(define-syntax _
  (lambda (x)
    (syntax-violation '_ "bad use of '_' syntactic keyword" x x)))

(define-syntax or
    (lambda (stx)
        (syntax-case stx ()
            [(_ e1 e2 e3 ...) 
                #'(let ([tmp e1]) (if tmp tmp (or e2 e3 ...)))]
            [(_ e) #'e]
            [(_) #'#f])))

(define-syntax and
    (lambda (stx)
        (syntax-case stx ()
            [(_ e1 e2 e3 ...) 
                #'(if e1 (and e2 e3 ...) #f)]
            [(_ e) #'e]
            [(_) #'#t])))
(print (and 1 2 3))